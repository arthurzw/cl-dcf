(in-package :com.google.catharsis)

(defclass test-definition (definition)
  ((value :accessor value :initarg :value :initform nil)))

(test "env-low-level"
      '(foo foo "bar-1" "bar-2" nil baz test-definition test-definition-1
	(foo bar bar baz test-definition-1 test-definition))
      (let* ((*temp-sym-counter* 0)
	     (env-1 (make-env))
	     (env-2 (make-local-env :parent env-1))
	     (foo (make-instance 'test-definition :id 'foo))
	     (bar-1 (make-instance 'test-definition :id 'bar :value "bar-1"))
	     (bar-2 (make-instance 'test-definition :id 'bar :value "bar-2"))
	     (baz (make-instance 'test-definition :id 'baz))
	     (fubar-1 (make-instance 'test-definition :id (gen-temp-sym)))
	     (fubar-2 (make-instance 'test-definition :id (gen-temp-sym))))
	(store-def foo env-1)
	(store-def bar-1 env-1)
	(store-def bar-2 env-2)
	(store-def baz env-2)
	(store-def fubar-1 env-2)
	(store-def fubar-2 env-2)
	(generate-missing-ids env-1)
	(list (aand (lookup-def 'foo env-1) (id it))
	      (aand (lookup-def 'foo env-2) (id it))
	      (aand (lookup-def 'bar env-1) (value it))
	      (aand (lookup-def 'bar env-2) (value it))
	      (aand (lookup-def 'baz env-1) (id it))
	      (aand (lookup-def 'baz env-2) (id it))
	      (aand (lookup-def 'test-definition env-2) (id it))
	      (aand (lookup-def 'test-definition-1 env-2) (id it))
	      (mapcar #'id (find-all-defs-of-type 'test-definition env-1)))))

(test "env-high-level" ; TODO: make this test look like "env-low-level"
      '(:form-1 :form-1-1 :form-1 :form-2 :form-1 nil :form-2 :form-2-1)
      (let ((*env* (make-env))
	    (result nil))
	(with-local-env
	  (store-def (make-instance 'definition :id :form-1))
	  (with-local-env
	    (store-def (make-instance 'definition :id :form-1-1))
	    (append1f result (lookup-def :form-1))
	    (append1f result (lookup-def :form-1-1)))
	  (append1f result (lookup-def :form-1))
	  (store-def (make-instance 'definition :id :form-2))
	  (append1f result (lookup-def :form-2))
	  (with-local-env
	    (store-def (make-instance 'definition :id :form-2-1))
	    (append1f result (lookup-def :form-1))
	    (append1f result (lookup-def :form-1-1))
	    (append1f result (lookup-def :form-2))
	    (append1f result (lookup-def :form-2-1))))
	(mapcar #'(lambda (x) (when x (id x))) result)))

(test "normalize-zexp-1"
      '(id tag (body1) (body2))
      (normalize-zexp '(def-tag id (body1) (body2))))

(test "normalize-zexp-2"
      '(id tag (body1) (body2))
      (normalize-zexp '(def id (tag (body1) (body2)))))

(test "normalize-zexp-3"
      '(com.google.catharsis.temp-syms::t-1 tag (body1) (body2))
      (let ((*temp-sym-counter* 0))
	(normalize-zexp '(tag (body1) (body2)))))

(test "normalize-zexp-4"
      '(id tag (value value) (body1) (body2))
      (normalize-zexp '(def-tag id value (body1) (body2))))

(test "normalize-zexp-5"
      '(id tag (value 'quoted-value) (body1) (body2))
      (normalize-zexp '(def-tag id 'quoted-value (body1) (body2))))

(test "normalize-zexp-6"
      '(id tag (value (list value)) (body1) (body2))
      (normalize-zexp '(def-tag id '(list value) (body1) (body2))))

;;;; DLS meta-description.

(compile-language dls

  (def-zexp-form internal-type
    (local-env)
    (def-prop class-name)
    (def-prop inherit (type internal-type) (repeating))
    (def-prop prop (type prop) (parse-by-type) (repeating)))

  (def-zexp-form zexp-type
    (inherit internal-type))

  (def-zexp-form zexp-form
    (inherit zexp-type)
    (def-prop local-env (flag) (slot-name 'local-env-p)))

  (def-zexp-form prop
    (class-name 'zexp-prop-definition)
    (def-prop type (accessor 'data-type))
    (def-prop flag (flag) (slot-name 'flag-p))
    (def-prop default-value)
    (def-prop repeating (flag) (slot-name 'repeating-p))
    (def-prop parse-by-type (flag) (slot-name 'parse-by-type-p))
    (def-prop slot-name (type 'symbol))
    (def-prop accessor (type 'symbol))
    (def-prop class-allocation (flag) (slot-name 'class-allocation-p)))

  (def-zexp-form sexp-form
    (inherit internal-type)
    (def-prop syntax))

  (def-zexp-form ir-overlay
    (def-prop ir-class (type ir-class) (parse-by-type) (repeating)))

  (def-zexp-form ir-class
    (inherit internal-type))

  (ir-overlay
   (def-ir-class internal-type
     (def-prop ir-class-name))
   (def-ir-class prop
     (def-prop ir-slot-name)
     (def-prop ir-accessor-name)
     (def-prop ir-lisp-data-type)
     (def-prop ir-user-data-type))))

;;;; S-expression parser infrastructure.

(compile-language sexp-parser

  (def-zexp-type sexp-syntax (class-name 'sexp-syntax))

  (def-zexp-form sexp-syntax-const (inherit sexp-syntax) (class-name 'sexp-syntax-const)
    (def-prop value))

  (def-zexp-form sexp-syntax-prop (inherit sexp-syntax) (class-name 'sexp-syntax-prop)
    (def-prop name))

  (def-zexp-form sexp-syntax-exp (inherit sexp-syntax-prop) (class-name 'sexp-syntax-exp) (local-env)
    (def-prop type-class)
    (def-prop body (type sexp-syntax) (parse-by-type) (repeating))))

;;;; Test language.

(compile-language ui

  (def-prop title (type 'string))

  (def-internal-type internal-value
    (def-prop type (accessor 'data-type)))

  (def-zexp-type data-type
    (def-prop bar (type 'integer) (repeating) (accessor '/bar/)))

  (def-zexp-form foo (inherit data-type)
    (def-prop baz (type form) (parse-by-type) (repeating))
    (def-prop bazn (type form))
    (def-prop bag (type 'integer) (default-value 666)))

  (def-zexp-form form
    title
    (def-prop modal (type 'boolean) (flag))
    (def-prop bar (type (def-zexp-form foo (def-prop xxx)))))

  (def-sexp-form + (syntax '(+ (* <arg>))))

  (def-zexp-form sexp-container
    (def-prop sexp)
    (def-prop sexp-by-type (type +) (parse-by-type)))

  (ir-overlay
   (def-ir-class form
     (def-prop class-name))))

(test "parse-simple-construct"
      '((form-definition
	 (bar (form-bar-foo-definition (env :suppressed)
				       (id form-bar-foo-definition)
				       (local-env nil)
				       (parent :recursive-reference)
				       (xxx 1)
				       (zexp (foo (xxx 1)))))
	 (class-name nil)
	 (env :suppressed)
	 (id foo)
	 (local-env nil)
	 (modal t)
	 (parent nil)
	 (title "fubar")
	 (zexp (def-form foo (title "fubar") (modal) (bar (foo (xxx 1)))))))
      (inspect-parse-tree
       +ui-language+
       '((def-form foo (title "fubar") (modal) (bar (foo (xxx 1)))))))

(test "parser-test-1"
      '((foo-definition (bag 666) (bar-list (0 1 2 -1))
	 (baz-list
	  ((form-definition (bar nil) (class-name nil) (env :suppressed) (id form-1)
			    (local-env nil) (modal nil) (parent :recursive-reference) (title "form-1"))
	   (form-definition (bar nil) (class-name nil) (env :suppressed) (id form-2)
			    (local-env nil) (modal nil) (parent :recursive-reference) (title "form-2"))))
	 (bazn
	  (form-definition (bar nil) (class-name nil) (env :suppressed)
			   (id form-embedded) (local-env nil) (modal nil)
			   (parent :recursive-reference) (title "embedded")))
	 (env :suppressed) (id foo-definition) (local-env nil) (parent nil)))
      (inspect-parse-tree
       +ui-language+
       '((foo
	  (bar 0) (bar 1) (bar 2) (bar -1)
	  (bazn (def-form form-embedded (title "embedded")))
	  (def-form form-1 (title "form-1"))
	  (def form-2 (form (title "form-2")))))))

(test "parser-test-2"
      '((foo-definition (bag 666) (bar-list (0 1 2 -1))
	 (baz-list
	  ((form-definition (bar nil) (class-name nil) (env :suppressed)
			    (id form-1) (local-env nil) (modal nil) (parent nil)
			    (title "form-1"))
	   (form-definition (bar nil) (class-name nil) (env :suppressed)
			    (id form-2) (local-env nil) (modal nil)
			    (parent :recursive-reference) (title "form-2"))))
	 (bazn
	  (form-definition (bar nil) (class-name nil) (env :suppressed)
			   (id form-1) (local-env nil) (modal nil) (parent nil)
			   (title "form-1")))
	 (env :suppressed) (id foo-definition) (local-env nil) (parent nil))
	(form-definition (bar nil) (class-name nil) (env :suppressed)
	 (id form-1) (local-env nil) (modal nil) (parent nil)
	 (title "form-1"))
	(sexp-container-definition (env :suppressed)
	 (id sexp-container-definition) (local-env nil) (parent nil)
	 (sexp
	  (+-definition (arg-list (1 2)) (env :suppressed) (id +-definition)
			(local-env nil) (parent :recursive-reference)))
	 (sexp-by-type
	  (+-definition (arg-list (3 4)) (env :suppressed) (id +-definition)
			(local-env nil) (parent :recursive-reference)))))
      (inspect-parse-tree
       +ui-language+
       '((foo
	  (bar 0) (bar 1) (bar 2) (bar -1)
	  (bazn form-1)
	  form-1
	  (def form-2 (form (title "form-2"))))
	 (def-form form-1 (title "form-1"))
	 (sexp-container (sexp (+ 1 2)) (+ 3 4)))))

;;;; s-expression syntax def

;;; sexp-definition   : base class
;;; symbol-definition, number-definition, list-definition.
;;; def-sexp-type     : provides classification (empty base classes)
;;; def-sexp-fragment : equivalent to naked rules in grammar.lisp.
;;; def-sexp-form     : equivalent to regular rules in grammar.lisp.

;; todo: read up on scheme's defsyntax
;; <foo> == (<foo> sexp)
;; Support properties with literal syntax (i.e. as if values were always quoted).
;; Such properties may be list-valued, as in (prop this is a list value) => '(this is a list value).

(def-sexp-form + (syntax '(repeating (<arg> sexp)))) ; => +-definition with property arg-list.
(def-sexp-form setf (syntax '(seq (<place> sexp) (<value> sexp))))
(def-sexp-form funcall (syntax '(seq (<fn> sexp) (repeating (<arg> sexp)))))

;;;;

(test "simple-list-parser"
      '((test-list (value 1) (value 2)))
      (block test
	(let* ((const-pd (make-instance 'constant-parser-def))
	       (list-pd (make-instance 'list-parser-def
				       :zexp-tag 'test-list
				       :nested-parser-def-list
				       (list (make-instance 'constant-parser-def
							    :ignore-value t
							    :expected-value 'expected)
					     (make-instance 'ref-parser-def :name 'value
							    :zexp-tag 'value)
					     (make-instance 'ref-parser-def :name 'value
							    :zexp-tag 'value))))
	       (*sexp-language* (list (cons 'value const-pd)
				      (cons 'list list-pd)))
	       (ps (make-parser-state list-pd)))
	  (setf (success-continuation ps) #'(lambda (value) (return-from test value)))
	  (process-sexp list-pd ps '(expected 1 2)) ; TODO: appending 3 makes the test pass.
	  :failed)))

(test "repeating-parser"
      '((test-list (value-list (test-number 1)
			       (test-number 2)
			       (test-number 3))))
      (block test
	(let* ((number-pd (make-instance 'number-parser-def :zexp-tag 'test-number))
	       (list-pd (make-instance 'list-parser-def
				       :zexp-tag 'test-list
				       :nested-parser-def-list
				       (list (make-instance 'constant-parser-def
							    :ignore-value t
							    :expected-value 'expected)
					     (make-instance 'repeating-parser-def
							    :zexp-tag 'value-list
							    :base-parser-def
							    (make-instance 'ref-parser-def :name 'value)))))
	       (*sexp-language* (list (cons 'value number-pd)
				      (cons 'list list-pd)))
	       (ps (make-parser-state list-pd)))
	  (setf (success-continuation ps) #'(lambda (value) (return-from test value)))
	  (process-sexp list-pd ps '(expected 1 2 3))
	  :failed)))

(test "recursive-parser"
      '((test-+
	 (arg 42)
	 (arg (test-+ (arg 1) (arg 7) (arg 9)))
	 (arg (test-+ (arg (test-+ (arg 2) (arg 3)))))))
      (block test
	(let* ((number-pd (make-instance 'number-parser-def))
	       (+-pd (make-instance 'list-parser-def
				    :zexp-tag 'test-+
				    :nested-parser-def-list
				    (list (make-instance 'constant-parser-def
							 :ignore-value t
							 :expected-value '+)
					  (make-instance 'repeating-parser-def
							 :base-parser-def
							 (make-instance 'ref-parser-def :name 'sexp
									:zexp-tag 'arg)))))
	       (sexp-pd (make-instance 'one-of-parser-def
				       :nested-parser-def-list (list number-pd +-pd)))
	       (*sexp-language* (list (cons 'sexp sexp-pd)
				      (cons 'number number-pd)
				      (cons '+ +-pd)))
	       (ps (make-parser-state sexp-pd)))
	  (setf (success-continuation ps) #'(lambda (value) (return-from test value)))
	  (process-sexp sexp-pd ps '(+ 42 (+ 1 7 9) (+ (+ 2 3)))) ; TODO: wrapping 2 3 into a list '(2 3) causes problems.
	  :failed)))

(test "optional-parser"
      '(((test-list (value 1) (value "optional-string") (value 2)))
	((test-list (value 1) (value 2))))
      (flet ((test (input)
	       (let* ((const-pd (make-instance 'constant-parser-def))
		      (list-pd (make-instance 'list-parser-def
					      :zexp-tag 'test-list
					      :nested-parser-def-list
					      (list (make-instance 'constant-parser-def
								   :ignore-value t
								   :expected-value 'expected)
						    (make-instance 'ref-parser-def :name 'value
								   :zexp-tag 'value)
						    (make-instance 'optional-parser-def
								   :base-parser-def
								   (make-instance 'string-parser-def
										  :zexp-tag 'value))
						    (make-instance 'ref-parser-def :name 'value
								   :zexp-tag 'value))))
		      (*sexp-language* (list (cons 'value const-pd)
					     (cons 'list list-pd)))
		      (ps (make-parser-state list-pd)))
		 (setf (success-continuation ps) #'(lambda (value) (return-from test value)))
		 (process-sexp list-pd ps input)
		 :failed)))
	(list (test '(expected 1 "optional-string" 2))
	      (test '(expected 1 2)))))

(test "compile-data-model-1"
      '(sexp-model-class
	(name +)
	(nested-class-list nil)
	(prop-list ((sexp-model-prop (name arg) (repeating t)))))
      (object->sexp
       (compile-data-model
	(first
	 (parse-program +sexp-parser-language+
			(parse-sexp-syntax '(<+> list + (* <arg>))))))))

(test "compile-parser-def-1"
      '(make-instance 'list-parser-def
	:zexp-tag '(+)
	:nested-parser-def-list
	(list
	 (make-instance 'constant-parser-def :expected-value '+ :ignore-value t)
	 (make-instance 'repeating-parser-def
	  :base-parser-def (make-instance 'ref-parser-def :name 'sexp :zexp-tag '(arg)))))
      (compile-parser-def
       (first
	(parse-program
	 +sexp-parser-language+
	 (parse-sexp-syntax '(<+> list + (* <arg>)))))))

(test "compile-data-model-2"
      '(sexp-model-class
	(name if)
	(nested-class-list nil)
	(prop-list ((sexp-model-prop (name test) (repeating nil))
		    (sexp-model-prop (name then-exp) (repeating nil))
		    (sexp-model-prop (name else-exp) (repeating nil)))))
      (object->sexp
       (compile-data-model
	(first
	 (parse-program
	  +sexp-parser-language+
	  (parse-sexp-syntax
	   '(<if> list if <test> <then-exp> (? <else-exp>))))))))

(test "compile-parser-def-2"
      '(make-instance 'list-parser-def
	:zexp-tag '(if)
	:nested-parser-def-list
	(list
	 (make-instance 'constant-parser-def :expected-value 'if :ignore-value t)
	 (make-instance 'ref-parser-def :name 'sexp :zexp-tag '(test))
	 (make-instance 'ref-parser-def :name 'sexp :zexp-tag '(then-exp))
	 (make-instance 'optional-parser-def
	  :base-parser-def (make-instance 'ref-parser-def :name 'sexp :zexp-tag '(else-exp)))))
      (compile-parser-def
       (first
	(parse-program
	 +sexp-parser-language+
	 (parse-sexp-syntax
	  '(<if> list if <test> <then-exp> (? <else-exp>)))))))

(test "compile-data-model-3"
      '(sexp-model-class
	(name case)
	(nested-class-list
	 ((sexp-model-class (name case-clause)
			    (nested-class-list nil)
			    (prop-list ((sexp-model-prop (name cmp-val) (repeating nil))
					(sexp-model-prop (name body) (repeating t)))))))
	(prop-list ((sexp-model-prop (name test) (repeating nil))
		    (sexp-model-prop (name clause) (repeating t)))))
      (object->sexp
       (compile-data-model
	(first
	 (parse-program
	  +sexp-parser-language+
	  (parse-sexp-syntax
	   '(<case> list case <test>
	     (+ (<clause> list (<cmp-val> constant) (* <body>))))))))))

(test "compile-parser-def-3"
      '(make-instance 'list-parser-def
	:zexp-tag '(case)
	:nested-parser-def-list
	(list
	 (make-instance 'constant-parser-def :expected-value 'case :ignore-value t)
	 (make-instance 'ref-parser-def :name 'sexp :zexp-tag '(test))
	 (make-instance 'repeating-parser-def :min-occurs 1
	  :base-parser-def
	  (make-instance 'list-parser-def
			 :zexp-tag '(case-clause clause)
			 :nested-parser-def-list
			 (list
			  (make-instance 'ref-parser-def :name 'constant :zexp-tag '(cmp-val))
			  (make-instance 'repeating-parser-def
					 :base-parser-def
					 (make-instance 'ref-parser-def :name 'sexp :zexp-tag '(body))))))))
      (compile-parser-def
       (first
	(parse-program
	 +sexp-parser-language+
	 (parse-sexp-syntax
	  '(<case> list case <test>
	    (+ (<clause> list (<cmp-val> constant) (* <body>)))))))))

(test "compile-data-model-4"
      '(sexp-model-class
	(name handler)
	(nested-class-list
	 ((sexp-model-class (name handler-param)
			    (nested-class-list nil)
			    (prop-list ((sexp-model-prop (name kind) (repeating nil))
					(sexp-model-prop (name value) (repeating nil)))))))
	(prop-list ((sexp-model-prop (name id) (repeating nil))
		    (sexp-model-prop (name param) (repeating t))
		    (sexp-model-prop (name body) (repeating t)))))
      (object->sexp
       (compile-data-model
	(first
	 (parse-program
	  +sexp-parser-language+
	  (parse-sexp-syntax
	   '(<handler> list handler
	     (? (<id> symbol))
	     (list (* (<param> list (<kind> symbol) (<value> symbol))))
	     (* <body>))))))))

(test "compile-parser-def-4"
      '(make-instance 'list-parser-def
	:zexp-tag '(handler)
	:nested-parser-def-list
	(list
	 (make-instance 'constant-parser-def :expected-value 'handler :ignore-value t)
	 (make-instance 'optional-parser-def
	  :base-parser-def (make-instance 'ref-parser-def :name 'symbol :zexp-tag '(id)))
	 (make-instance 'list-parser-def
	  :nested-parser-def-list
	  (list
	   (make-instance 'repeating-parser-def
			  :base-parser-def
			  (make-instance 'list-parser-def
					 :zexp-tag '(handler-param param)
					 :nested-parser-def-list
					 (list
					  (make-instance 'ref-parser-def :name 'symbol :zexp-tag '(kind))
					  (make-instance 'ref-parser-def :name 'symbol :zexp-tag '(value)))))))
	 (make-instance 'repeating-parser-def
	  :base-parser-def (make-instance 'ref-parser-def :name 'sexp :zexp-tag '(body)))))
      (compile-parser-def
       (first
	(parse-program
	 +sexp-parser-language+
	 (parse-sexp-syntax
	  '(<handler> list handler
	    (? (<id> symbol))
	    (list (* (<param> list (<kind> symbol) (<value> symbol))))
	    (* <body>)))))))

(test "translate-sexp-form"
      '(<+> list + (* <arg>))
      (translate-sexp-form (make-instance 'sexp-form-definition
					  :id '+
					  :syntax '(+ (* <arg>)))))

(test "compile-sexp-language-parser"
      '(let* ((+-pd (make-instance 'list-parser-def :zexp-tag '(+)
				   :nested-parser-def-list
				   (list (make-instance 'constant-parser-def :expected-value '+ :ignore-value t)
					 (make-instance 'repeating-parser-def
							:base-parser-def
							(make-instance 'ref-parser-def
								       :name 'sexp :zexp-tag '(arg))))))
	      (sexp-top-level-pd (make-instance 'one-of-parser-def
						:nested-parser-def-list
						(list (make-instance 'ref-parser-def :name '+)
						      (make-instance 'ref-parser-def :name 'number)
						      (make-instance 'ref-parser-def :name 'string)
						      (make-instance 'ref-parser-def :name 'symbol))))

	       (sexp-language (list (cons 'sexp sexp-top-level-pd)
				    (cons 'number (make-instance 'constant-parser-def :expected-type 'number))
				    (cons 'string (make-instance 'constant-parser-def :expected-type 'string))
				    (cons 'symbol (make-instance 'constant-parser-def :expected-type 'atom))
				    (cons '+ +-pd))))
	  (defun parse-sexp-test (sexp)
	    (let* ((*sexp-language* sexp-language)
		   (ps (make-parser-state sexp-top-level-pd)))
	      (setf (success-continuation ps)
		    #'(lambda (value)
			(return-from parse-sexp-test (first value))))
	      (process-sexp sexp-top-level-pd ps sexp)
	      nil)))
      (compile-sexp-language-parser
       :test
       (preprocess-sexp-language (list (make-instance 'sexp-form-definition
						      :id '+
						      :syntax '(+ (* <arg>)))))))

;;;;

<xxx> == (<xxx> sexp)

sexp-token ::= constant
           ::= sexp-var
           ::= (list (? (<name> sexp-var))
		     (<type-class> one-of '* '+ '? 'one-of 'list 'sexp 'symbol 'number 'string)
		     (* <sexp-token>))

(def-sexp-form + (syntax '(+ (* <arg>)))) ; => +-definition with property arg-list.
;; - * / mod and or
(def-sexp-form setf (syntax '(setf <place> <value>)))
(def-sexp-form funcall (syntax '(funcall <fn> (* <arg>))))
(def-sexp-form lambda (syntax '(lambda (list (* (<param> symbol))) (* <body>))))
(def-sexp-form case (syntax '(case <test> (+ (<clause> list (<cmp-val> constant) (* <body>))))))
(def-sexp-form ecase (inherit case))
;; typecase etypecase
(def-sexp-form cond (syntax '(cond (* (<clause> list <test> (* <body>))))))
(def-sexp-form if (syntax '(if <test> <then-exp> (? <else-exp>))))
(def-sexp-form syntactic-call (syntax '((<id> symbol) (* <arg>))))
(def-internal-type function-call (inherit syntactic-call) (def-prop function) ...)
(def-internal-type template-call (inherit syntactic-call) (def-prop template) ...)
(def-internal-type macro-call (inherit syntactic-call) (def-prop macro) ...)
(def-sexp-form handler (syntax '(handler (? (<id> symbol))
				 (list (* (<param> list (<kind> symbol) (<value> symbol))))
				 (* <body>))))

#|

'((def counter-value (var (int) (default-value 0)))
  (def up-down-counter-sample
      (form
       (title "Up/Down Counter -- Sample")
       (horizontal-layout
	(def display (text (value counter-value)))
	(def up (button (label "Up")))
	(def down (button (label "Down")))
	(def reset (button (label "Reset")
			   (handler ((type clicked))
				    (setf counter-value 0))))
	(handler ((widget up) (type clicked))
		 (incf counter-value))
	(handler ((widget down) (type clicked))
		 (decf counter-value))
	(handler ()
		 (log "This illustrates a catch-all handler."))))))

;;;; base.dls.lisp

(internal-type :value
	       (prop :type))

(internal-type :constant (inherit value))

(internal-type :var
	       (doc "A procedure or template parameter, or a local variable."))

; variable (definition)
; procedure -- sequential content model

;;;; lisp.dls.lisp

;(sexp-form expr ())
;(sexp-form if-expr ())
;...
(sexp-form lambda-expr ()
	   (syntax lambda <lambda-list> <expr>))

;;;; data.dls.lisp

(zexp-type :data-type)
(zexp-type :simple-type (inherit data-type))
(zexp-type :complex-type (inherit data-type))

(zexp-type :length-mixin
	   (prop :min-length (type (integer 0)))
	   (prop :max-length (type (integer 0))))

(zexp-form :bool (inherit simple-type))
(zexp-form :id (inherit simple-type))

(zexp-form :int (inherit simple-type)
	   (prop :min-value (type integer))
	   (prop :max-value (type integer)))

(zexp-form :string (inherit simple-type length-mixin))
(zexp-form :uri (inherit simple-type length-mixin))

(internal-type :enum-value (inherit value))
(zexp-form :enum (inherit simple-type)
	   (prop :values (accessor enum-values) (type (list value))))

(zexp-form :optional (inherit complex-type)
	   (prop :base-type (type data-type) (parse-by-type)))

(zexp-type :collection (inherit complex-type)
	   (prop :item-type (type data-type) (parse-by-type))
	   (prop :max-size (type (integer 0))))

(zexp-form :set (inherit collection))
(zexp-form :list (inherit collection))

(zexp-form :var
	   (prop :type (type data-type) (parse-by-type))
	   (prop :default-value)
	   (prop :lambda-expr (zexp-tag 'lambda) (type lambda-expr)))

(zexp-type :var-container
	   (prop :var (repeating) (type var) (parse-by-type)))

;;;; tsm.dls.lisp

(zexp-form :procedure
	   (prop :body (type (list <expr>)))) -- problem. see handler.

(zexp-type :procedure-container
	   (prop :procedure (repeating) (type procedure) (parse-by-type)))

(zexp-form :handler (inherit procedure) -- problem. the syntax is sexpr.
	   (prop :predicate-list))

(zexp-type :handler-container
	   (prop :handler (repeating) (type handler) (parse-by-type)))

;;;; ui.dls.lisp

(zexp-type :widget (inherit handler-container))

(zexp-type :widget-container
	   (prop :widget (repeating) (type widget) (parse-by-type)))

(sexp-form :template ...)

(zexp-type :template-container
	   (prop :template (repeating) (type template) (parse-by-type)))

(sexp-form :template-invocation
	   (syntax <template-ref> (* <param>)))

(zexp-form :form (inherit widget field-container procedure-container widget-container prototype-container template-container)
	   (prop :title (type (string)) (data-bound)))

(zexp-form :text (inherit widget)
	   (prop :value (data-bound)))

(zexp-form :button (inherit widget)
	   (prop :label (data-bound)))

(zexp-form :checkbox (inherit widget)
	   (prop :label (data-bound))
	   (prop :value (data-bound)))

(zexp-form :horizontal-layout (inherit widget-container widget))
(zexp-form :vertical-layout (inherit widget-container widget))

;;;; orchestration.dls.lisp
|#
