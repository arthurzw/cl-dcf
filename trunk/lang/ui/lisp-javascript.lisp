(in-package :com.google.catharsis)


;;;; Local utilities.

(defun namespaced-symbol->keyword (sym)
  "Converts a symbol into a keyword, including the symbol's package.
The prefix comes from the symbol package's nickname, if one is found."
  (gen-symbol :keyword
              (first (package-nicknames (symbol-package sym)))
              sym))

(test "namespaced-symbol->keyword"
      :ui-widget
      (namespaced-symbol->keyword 'widget))


(defun js-symbol-name (sym)
  "Generates a JavaScript-compatible name for the specified symbol."
  (let ((sym-name (symbol-name sym)))
    (substitute #\_ #\-
		(symbol-name
		 (apply #'gen-symbol
			:keyword
			(when (find (char sym-name 0) "0123456789") #\_)
			(mapcar #'(lambda (x)
				    (acond ((find x "ABCDEFGHIJKLMNOPQRSTUVWXYZ01234567890_$@") x)
					   ((case x
					      (#\+ "PLUS")
					      (#\- "MINUS")
					      (#\* "STAR")
					      (#\/ "SLASH")
					      (#\! "BANG")
					      (#\~ "TILDE")
					      (#\# "SHARP")
					      (#\% "PERCENT")
					      (#\^ "HAT")
					      (#\& "AMPERSAND")
					      (#\= "EQUAL")
					      (#\? "QUESTION")
					      (#\< "LESS_THAN")
					      (#\> "GREATER_THAN")
					      (#\. "PERIOD")
					      (#\, "COMMA"))
					    it)
					   (t (error "Invalid symbol character ~A." x))))
				(concatenate 'list sym-name)))))))

(test "js-symbol-name"
      "AB_CD_MINUS"
      (js-symbol-name :ab-cd-))


;;;; Expressions.

(defvar *expr-deps* nil "List of field dependencies of the currently compiled expression.")

(defgeneric compile-expr~ (tag expr))

(defun compile-expr (expr)
  (if (listp expr)
      (compile-expr~ (first expr) expr)
      (compile-expr~ nil expr)))


(defmethod compile-expr~ ((tag (eql nil)) (val (eql nil)))
  :undefined)


(defmethod compile-expr~ ((tag (eql nil)) (sym symbol))
  (if (keywordp sym)
      (js-symbol-name sym)
      (etypecase (lookup-definition sym)
	(field-definition
	 (prog1 `(js:call (js:field-ref-self ,(gen-symbol :keyword sym)) :get)
	   (push sym *expr-deps*)))
	(variable-definition
	 `(js:field-ref-self ,(gen-symbol :keyword sym))))))

(test "compile-symbol"
      '(call (field-ref-self :symbol) :get)
      (compile-expr 'symbol))


(defmethod compile-expr~ ((tag (eql nil)) (num number))
  num)

(test "compile-number"
      42
      (compile-expr 42))


(defmethod compile-expr~ ((tag (eql nil)) (str string))
  `(string-literal ,str))

(test "compile-string"
      '(string-literal "fubar")
      (compile-expr "fubar"))


(defmethod compile-expr~ ((tag (eql 'var-ref)) var-ref)
  (second var-ref))

(test "compile-var-ref"
      :var1
      (compile-expr '(var-ref :var1)))


(defmethod compile-expr~ ((tag (eql 'quote)) quote)
  (gen-symbol :keyword (second quote)))

(test "compile-quote"
      :symbol
      (compile-expr ''symbol))


(defmethod compile-expr~ ((tag (eql 'function)) fun)
  (ecase (second fun)
    (+ :add) ;; TODO: we probably want to emit global.add to avoid name shadowing to local symbols.
    (- :subtract)
    (* :multiply)
    (/ :divide)))

(test "compile-literal-fun"
      :add
      (compile-expr '#'+))


(defmethod compile-expr~ ((tag (eql 'funcall)) funcall)
  `(funcall ,@(mapcar #'compile-expr (rest funcall))))

(test "compile-funcall"
      '(funcall :add
	        (call (field-ref-self :a) :get)
	        (call (field-ref-self :b) :get))
      (let ((*env* nil))
	(with-local-env
	  (store-definition (make-instance 'field-definition :name :a))
	  (store-definition (make-instance 'field-definition :name :b))
	  (compile-expr '(funcall #'+ a b)))))


(macrolet ((std-fun (tag js-fun)
	     `(defmethod compile-expr~ ((tag (eql ,tag)) zexp)
	       `(funcall ,,js-fun ,@(mapcar #'compile-expr (rest zexp))))))
  (std-fun '+ :add)
  (std-fun '- :subtract)
  (std-fun '* :multiply)
  (std-fun '/ :divide))

(test "arithmetic"
      '(funcall :multiply 2 "p_i"
	(funcall :add
	 (call (field-ref-self :r1) :get)
	 (call (field-ref-self :r2) :get)))
      (let ((*env* nil))
	(with-local-env
	  (store-definition (make-instance 'field-definition :name :r1))
	  (store-definition (make-instance 'field-definition :name :r2))
	  (compile-expr '(* 2 :pi (+ r1 r2))))))
	;; TODO: this should be more efficient. use the environment to determine whether a
	;; symbol is a simple variable or a field reference.


(defmethod compile-expr~ ((tag (eql 'or)) or) ; TODO: use JavaScript ( ... || ... || ...); do this as a macro in javascript.lisp.
  (nlet rec ((args (mapcar #'compile-expr (rest or))))
    (let ((expr (first args))
	  (rest (rest args)))
      (if (null rest)
	  expr
	  `(thunk-call
	    (var :x ,expr)
	    (if (!= :x :undefined)
		(return :x)
		(return ,(rec rest))))))))

(test "compile-or"
      '(thunk-call
	(var :x (call (field-ref-self :a) :get))
	(if (!= :x :undefined)
	    (return :x)
	    (return
	      (thunk-call
	       (var :x (call (field-ref-self :b) :get))
	       (if (!= :x :undefined)
		   (return :x)
		   (return (call (field-ref-self :c) :get)))))))
      (let ((*env* nil))
	(with-local-env
	  (store-definition (make-instance 'field-definition :name :a))
	  (store-definition (make-instance 'field-definition :name :b))
	  (store-definition (make-instance 'field-definition :name :c))
	  (compile-expr '(or a b c)))))


(defmethod compile-expr~ ((tag (eql 'if)) if)
  (let ((condition (second if))
	(then (third if))
	(else (fourth if)))
    `(thunk-call
      (if ,(compile-expr condition)
	  (return ,(compile-expr then))
	  ,@(when else `((return ,(compile-expr else))))))))

(test "compile-if"
      '(thunk-call
	(if (call (field-ref-self :a) :get)
	    (return (call (field-ref-self :b) :get))
	    (return (call (field-ref-self :c) :get))))
      (let ((*env* nil))
	(with-local-env
	  (store-definition (make-instance 'field-definition :name :a))
	  (store-definition (make-instance 'field-definition :name :b))
	  (store-definition (make-instance 'field-definition :name :c))
	  (compile-expr '(if a b c)))))


(defmethod compile-expr~ ((tag (eql 'ecase)) ecase)
  `(thunk-call
    (switch ,(compile-expr (second ecase))
            ,@(mapcar (curry #'apply #'(lambda (val expr) `(case ,(compile-expr val) (return ,(compile-expr expr)))))
		      (drop 2 ecase))
            (default (return (string-literal "ecase error."))))))

(test "compile-ecase"
      '(thunk-call
	(switch (call (field-ref-self :y) :get)
	        (case :v1 (return (call (field-ref-self :calculate-v1) :get)))
		(case :v2 (return (call (field-ref-self :calculate-v2) :get)))
		(case :v3 (return (call (field-ref-self :calculate-v3) :get)))
		(default (return (string-literal "ecase error.")))))
      (let ((*env* nil))
	(with-local-env
	  (store-definition (make-instance 'field-definition :name :y))
	  (store-definition (make-instance 'field-definition :name :calculate-v1))
	  (store-definition (make-instance 'field-definition :name :calculate-v2))
	  (store-definition (make-instance 'field-definition :name :calculate-v3))
	  (compile-expr '(ecase y
			  (:v1 calculate-v1)
			  (:v2 calculate-v2)
			  (:v3 calculate-v3))))))


;;;; Statements

(defgeneric compile-stmt~ (tag stmt))

(defun compile-stmt (stmt)
  (if (listp stmt)
      (compile-stmt~ (first stmt) stmt)
      (compile-stmt~ nil stmt)))


(defmethod compile-stmt~ ((tag (eql 'setf)) setf) ;=> Macro
  `((call (field-ref-self ,(symbol->keyword (second setf))) :set
     ,(compile-expr (third setf)))))

(test "compile-setf"
      '((call (field-ref-self :a) :set (string-literal "new-value")))
      (compile-stmt '(setf a "new-value")))


(defmethod compile-stmt~ ((tag (eql 'incf)) incf)
  `((call (field-ref-self ,(symbol->keyword (second incf))) :set
     (+ ,(compile-expr (second incf))
        ,(aif (third incf)
	      (compile-expr it)
	      1)))))

(test "compile-incf"
      '((call (field-ref-self :a) :set
	 (+ (call (field-ref-self :a) :get) 1))
	(call (field-ref-self :a) :set
	 (+ (call (field-ref-self :a) :get) 2)))
      (append (compile-stmt '(incf a))
	      (compile-stmt '(incf a 2))))


(defmethod compile-stmt~ ((tag (eql 'decf)) decf)
  `((call (field-ref-self ,(symbol->keyword (second decf))) :set
     (- ,(compile-expr (second decf))
        ,(aif (third decf)
	      (compile-expr it)
	      1)))))

(test "compile-decf"
      '((call (field-ref-self :a) :set
	 (- (call (field-ref-self :a) :get) 1))
	(call (field-ref-self :a) :set
	 (- (call (field-ref-self :a) :get) 2)))
      (append (compile-stmt '(decf a))
	      (compile-stmt '(decf a 2))))


(defmethod compile-stmt~ ((tag (eql 'log)) log)
  `((// ,(second log))))


(defmethod compile-stmt~ (unknown-tag stmt)
  (error "Unknown statement ~S." stmt))
