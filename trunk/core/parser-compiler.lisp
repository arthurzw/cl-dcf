(in-package :com.google.catharsis)

;;; Meta-language compiler.

(defmethod post-process ((obj internal-type-definition))
  (when (next-method-p) (call-next-method))
  (setf (ir-class-name obj)
	(or (class-name obj)
	    (apply #'gen-symbol
		   nil;todo:(symbol-package (id obj))
		   ;; Prepend enclosing environment names for nested definitions.
		   (nlet rec ((acc (list (id obj) :definition)) (env (env obj)))
		     (aif (id env)
			  (rec (cons it acc) (parent env))
			  acc))))))

(defmethod verify-def ((obj zexp-prop-definition))
  (when (and (parse-by-type-p obj)
	     (not (data-type obj)))
    (error "Error in ~A: (parse-by-type) requires a (type ...) property." (id obj))))

(defmethod post-process ((obj zexp-prop-definition))
  (setf (ir-slot-name obj)
	(or (slot-name obj)
	    (if (repeating-p obj)
		(gen-symbol (symbol-package (id obj)) (id obj) :list)
		(id obj))))
  (setf (ir-accessor-name obj)
	(or (accessor obj) (ir-slot-name obj)))
  (cond ((typep (data-type obj) 'internal-type-definition)
	 (setf (ir-user-data-type obj) (data-type obj)))
	((typep (data-type obj) 'sexp-form-definition)
	 (setf (ir-lisp-data-type obj) (gen-symbol nil (id (data-type obj)) :definition)))
	(t
	 (setf (ir-lisp-data-type obj) (data-type obj)))))

(defun compile-dispatch-table (def-list)
  `(bulk-insert-trie
    ',(mapcar #'(lambda (def)
		  (list
		   ;; Build a stack context list by accumulating all containing zexp form definitions.
		   (nlet rec ((acc nil) (def def))
		     (cond ((null def) (nreverse acc))
			   ((typep def 'zexp-form-definition) (rec (cons (id def) acc) (parent def)))
			   (t (rec acc (parent def)))))
		   (ir-class-name def)))
	      (remove-if-not (rcurry #'typep 'zexp-form-definition) def-list))
    (make-instance 'trie)))

(defun compile-macro-dispatch-table (syntax-list)
  `(let ((mdt (make-hash-table)))
    ,@(mapcar #'(lambda (syntax)
		  `(setf (gethash ',(sexp-prop-name (name syntax)) mdt) :sexp))
	      syntax-list)
    mdt))

(defun compile-data-model-class (def)
  "Produces a class declaration."
  (assert (typep def 'internal-type-definition))
  (let* ((ir-overlay (lookup-def-by-type 'ir-overlay-definition))
	 (class-overlay (aand ir-overlay (find (id def) (ir-class-list it) :key #'id)))
	 (prop-list (append (prop-list def) (aand class-overlay (prop-list it)))))
    `((export ',(ir-class-name def))
      ,@(mapcar #'(lambda (prop) `(export ',(ir-accessor-name prop))) prop-list)
      (defclass ,(ir-class-name def)
	;; Base class list. If no inheritance is specified, inherit from the runtime's definition class.
	,(aif (append (inherit-list def) (aand class-overlay (inherit-list it)))
	      (mapcar #'ir-class-name it)
	      '(definition))
	,(list*
	  '(prop-tags :accessor prop-tags :initform nil :allocation :class)
	  (mapcar #'(lambda (prop)
		      (list (ir-slot-name prop)
			    :accessor (ir-accessor-name prop)
			    :initarg (make-keyword (ir-slot-name prop))
			    :initform nil
			    :allocation (if (class-allocation-p prop) :class :instance)))
		  prop-list)))
      (setf (prop-tags (make-instance ',(ir-class-name def)))
       ;; Compute the list of all properties, including those in base classes.
       ',(nlet rec ((acc nil) (class-stack (list def)))
	       (if (null class-stack)
		   acc
		   (rec (append (mapcar #'id (remove-if #'parse-by-type-p (prop-list (first class-stack)))) acc)
			(append (inherit-list (first class-stack)) (rest class-stack)))))))))

(defun compile-set-defaults-methods (def)
  "Produces a list of set-defaults methods for all properties of the given definition."
  (assert (typep def 'internal-type-definition))
  (awhen (remove-if-not #'default-value (prop-list def))
    `((defmethod set-defaults ((obj ,(ir-class-name def)))
	,@(mapcar #'(lambda (prop)
		      `(unless (,(ir-accessor-name prop) obj)
			(setf (,(ir-accessor-name prop) obj) ,(default-value prop))))
		  it)
	(when (next-method-p) (call-next-method))))))

(defun compile-parse-property-methods (def)
  "Produces a list of parse-*-property-* methods for all properties of the given definition."
  (assert (typep def 'internal-type-definition))
  (let ((class-id (ir-class-name def)))
    (append
     (mapcar #'(lambda (prop)
		 (let* ((lisp-data-type (ir-lisp-data-type prop))
			(user-data-type (ir-user-data-type prop))
			(method-header (if (parse-by-type-p prop)
					   `(parse-zexp-property-by-type ((obj ,class-id)
									  (def ,(cond (user-data-type
										       (ir-class-name user-data-type))
										      (lisp-data-type
										       lisp-data-type)
										      (t t)))))
					   `(parse-zexp-property-by-tag ((obj ,class-id)
									 (tag (eql ',(id prop)))
									 body)))))
		   `(defmethod ,@method-header
		     (let ((prop-value ,(cond ((parse-by-type-p prop) 'def)
					      ((flag-p prop) 't)
					      (t '(parse-zexp-prop-value-phase-2 obj (first body))))))
		       ;; If the syntax is a Lisp data type, verify it.
		       ,@(when (and lisp-data-type (not (eq lisp-data-type 'boolean)))
			       (assert (not (typep lisp-data-type 'standard-object)))
			       `((unless (typep prop-value ',lisp-data-type)
				   (error "Invalid value of property ~A. ~A expected."
					  ',(id prop) ',lisp-data-type))))
		       ;; If the syntax is a user data type, verify it.
		       ,@(when user-data-type
			       `((unless (typep prop-value ',(ir-class-name user-data-type))
				   (error "Invalid value of property ~A. ~A expected."
					  ',(id prop) ',(id user-data-type)))))
		       ;; If the syntax is a flag, make sure there's no additional data.
		       ;; TODO: support the extended syntax (flag value), e.g. (def-prop ... (flag nil)).
		       ,@(when (flag-p prop)
			       `((when body
				   (error "Property ~A is a simple boolean flag. The following declaration is incorrect: ~A."
					  ',(id prop) body))))
		       ,(if (repeating-p prop)
			    `(append1f (,(ir-accessor-name prop) obj) prop-value)
			    `(if (,(ir-accessor-name prop) obj)
			      (error "Duplicate instance of property ~A." ',(id prop))
			      (setf (,(ir-accessor-name prop) obj) prop-value)))))))
	     (prop-list def)))))

(defun compile-initialize-instance-method (def)
  "Produces an initialize-instance method for the zexp-form-definition."
  (assert (typep def 'zexp-form-definition))
  (when (local-env-p def)
    `((defmethod initialize-instance ((obj ,(ir-class-name def)) &rest initargs &key)
	(declare (ignore initargs))
	(call-next-method)
	(setf (local-env obj) (make-local-env :id (id obj)))))))

(defgeneric compile-def (phase def)
  (:documentation "Compiles a DLS definition."))

(defmethod compile-def (phase def) nil)

(defmethod compile-def ((phase (eql 1)) (def internal-type-definition))
  `(,@(when (next-method-p) (call-next-method))
    ,@(compile-data-model-class def)
    ,@(compile-set-defaults-methods def)))

(defmethod compile-def ((phase (eql 2)) (def zexp-type-definition))
  `(,@(when (next-method-p) (call-next-method))
    ,@(compile-parse-property-methods def)))

(defmethod compile-def ((phase (eql 2)) (def zexp-form-definition))
  `(,@(when (next-method-p) (call-next-method))
    ,@(compile-initialize-instance-method def)))

(defmethod compile-def ((phase (eql 1)) (def ir-class-definition))
  (declare (ignore phase def))
  nil)

(defmacro compile-language (name &body lang-def)
  (multiple-value-bind (top-level-defs all-defs *env*)
      (parse-program +dls-language+ lang-def)
    (declare (ignore top-level-defs))
    (let* ((sexp-defs (remove-if-not #'(lambda (x) (eq (type-of x) 'sexp-form-definition)) all-defs))
	   (sexp-syntax-list (preprocess-sexp-language sexp-defs))
	   (zexp-defs (append (remove 'sexp-form-definition all-defs :key #'type-of)
			      (compile-zexp-form-definitions sexp-syntax-list sexp-defs)))
	   (lang-name (gen-special-symbol (symbol-package name) :+ name :language)))
      (multiple-value-bind (sexp-parser sexp-parser-fn)
	  (compile-sexp-language-parser name sexp-syntax-list)
	`(progn
	  (export ',lang-name)
	  (define-constant ,lang-name
	    (make-instance 'language
			   :dispatch-table ,(compile-dispatch-table zexp-defs)
			   :macro-dispatch-table ,(compile-macro-dispatch-table sexp-syntax-list)
			   :sexp-parser-fn ',sexp-parser-fn))
	  ,@(mappend (curry #'compile-def 1) zexp-defs)
	  ,@(mappend (curry #'compile-def 2) zexp-defs)
	  ,@(when sexp-syntax-list (list sexp-parser)))))))
