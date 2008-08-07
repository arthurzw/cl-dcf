(in-package :com.google.catharsis)

;;;; Base sexp parser definition.

(defclass sexp-parser-def ()
  ((zexp-tag :reader zexp-tag :initarg :zexp-tag :initform nil
	     :documentation "Tag for the z-expression representation of this s-expression.")
   (ignore-value :reader ignore-value :initarg :ignore-value :initform nil
		 :documentation "Specifies whether the parser's value is to be ignored."))
  (:documentation "A static definition for an s-expression parser."))

(defclass sexp-parser-state ()
  ((def :reader def :initarg :def :documentation "Parser definition.")
   (parent-state :reader parent-state :initarg :parent-state :initform nil
		 :documentation "If this parser is invoked as part of another parser, this slot points to the parent's state.")
   (success-continuation :accessor success-continuation :initarg :success-continuation :initform nil
			 :documentation "Function invoked when the parser completes successfully.")
   (failure-continuation :accessor failure-continuation :initarg :failure-continuation :initform nil
			 :documentation "Function invoked when the parser fails. It is passed the error message."))
  (:documentation "Base class for s-expression parser state classes."))

(defvar *sexp-language* nil)

(defgeneric make-parser-state (parser-def &key parent-state)
  (:documentation "Creates a state object corresponding to the definition."))

(defgeneric process-sexp (parser-def parser-state sexp)
  (:documentation "Processes a sexp using the specified parser. If the parser completes, its continuation function is invoked."))

(define-condition unget-sexp (condition) ())

;;;; Helper routines.

(defun complete-parser (parser-state value &key singleton)
  (let* ((list-value (if singleton (list value) value))
	 (wrapped-value (unless (ignore-value (def parser-state))
			  (aif (zexp-tag (def parser-state))
			       (cons (nlet make-zexp ((tag-list (if (listp it) it (list it))))
				       (aif (rest tag-list)
					    (cons (first tag-list) (cons (make-zexp (rest tag-list)) nil))
					    (cons (first tag-list) list-value)))
				     nil)
			       list-value))))
    (awhen (success-continuation parser-state)
      (funcall it wrapped-value))
    wrapped-value))

(defun fail-parser (parser-state error-message &rest format-args)
  (awhen (failure-continuation parser-state) (funcall it (apply #'format nil error-message format-args))))

;;;; Referenced parser.

(defclass ref-parser-def (sexp-parser-def)
  ((name :reader name :initarg :name))
  (:documentation "A stub parser definition. When it's expanded, the name is looked up in *sexp-language*, which produces an effective parser definition."))

(defmethod print-object ((obj ref-parser-def) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "(name ~A)" (name obj))))

(defclass ref-parser-state (sexp-parser-state)
  ((inner-pd :accessor inner-pd :initarg :inner-pd)
   (inner-ps :accessor inner-ps :initform nil))
  (:documentation "A wrapper state object."))

(defmethod make-parser-state ((pd ref-parser-def) &key parent-state)
  (let ((inner-pd (cdr (assoc (name pd) *sexp-language*))))
    (unless inner-pd (error "No named definition ~A." (name pd)))
    (make-instance 'ref-parser-state :def pd :parent-state parent-state :inner-pd inner-pd)))

(defmethod process-sexp ((pd ref-parser-def) (ps ref-parser-state) sexp)
  ;; Lazily create inner state. This allows recursive definitions.
  (unless (inner-ps ps)
    (let ((inner-ps (make-parser-state (inner-pd ps) :parent-state ps)))
      (setf (inner-ps ps) inner-ps)
      (setf (success-continuation inner-ps)
	    #'(lambda (value)
		(complete-parser ps value)))
      (setf (failure-continuation inner-ps)
	    #'(lambda (error-message)
		(fail-parser ps error-message)))))
  (process-sexp (def (inner-ps ps)) (inner-ps ps) sexp))

;;;; Constant parser and subclasses.

(defclass constant-parser-def (sexp-parser-def)
  ((expected-value :accessor expected-value :initarg :expected-value)
   (expected-type :accessor expected-type :initarg :expected-type :initform nil)))

(defmethod print-object ((obj constant-parser-def) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (when (slot-boundp obj 'expected-value)
      (format stream "(expected-value ~A)" (expected-value obj)))))

(defclass constant-parser-state (sexp-parser-state) ())

(defmethod make-parser-state ((pd constant-parser-def) &key parent-state)
  (make-instance 'constant-parser-state :def pd :parent-state parent-state))

(defmethod process-sexp ((pd constant-parser-def) (ps constant-parser-state) sexp)
  (flet ((complete-parser-with-check (value)
	   (if (aand (expected-type pd)
		     (not (typep value it)))
	       (fail-parser ps "Unexpected value (incorrect type).")
	       (let ((quoted-value (if (or (listp value)
					   (symbolp value))
				       (list 'quote value)
				       value)))
		 (complete-parser ps quoted-value :singleton t)))))
    (if (slot-boundp pd 'expected-value)
	(if (eq sexp (expected-value pd))
	    (complete-parser-with-check sexp)
	    (fail-parser ps "~A expected." (expected-value pd)))
	(if (listp sexp)
	    (if (eq (first sexp) 'quote)
		(complete-parser-with-check (second sexp))
		(fail-parser ps "Unexpected list. Only a quoted list is acceptable in this context."))
	    (complete-parser-with-check sexp)))))

;; TODO: reimplement stuff below in terms of constant-parser-def

(defun process-sexp-with-check (pd ps sexp check-fn error-message)
  "Utility function for use by subclasses of constant-parser."
  (let ((quoted-sexp (if (or (listp sexp)
			     (symbolp sexp))
			 (list 'quote sexp)
			 sexp)))
    (if (slot-boundp pd 'expected-value)
	(if (eq sexp (expected-value pd))
	    (complete-parser ps quoted-sexp :singleton t)
	    (fail-parser ps "~A expected." (expected-value pd)))
	(if (funcall check-fn sexp)
	    (complete-parser ps quoted-sexp :singleton t)
	    (fail-parser ps error-message)))))

(defclass symbol-parser-def (constant-parser-def) ())

(defmethod process-sexp ((pd symbol-parser-def) (ps constant-parser-state) sexp)
  (process-sexp-with-check pd ps sexp #'symbolp "Symbol expected."))

(defclass number-parser-def (constant-parser-def) ())

(defmethod process-sexp ((pd number-parser-def) (ps constant-parser-state) sexp)
  (process-sexp-with-check pd ps sexp #'numberp "Number expected."))

(defclass string-parser-def (constant-parser-def) ())

(defmethod process-sexp ((pd string-parser-def) (ps constant-parser-state) sexp)
  (process-sexp-with-check pd ps sexp #'stringp "String expected."))

;;;; Sequential parser.

(defclass seq-parser-def (sexp-parser-def)
  ((nested-parser-def-list :accessor nested-parser-def-list :initarg :nested-parser-def-list :initform nil
			   :documentation "A list of parsers to process in sequence.")))

(defmethod print-object ((obj seq-parser-def) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~{~I~A~}" (nested-parser-def-list obj))))

(defclass seq-parser-state (sexp-parser-state)
  ((nested-parser-state-list :accessor nested-parser-state-list :initarg :nested-parser-state-list :initform nil)
   (value :accessor value :initarg :value :initform nil)))

(defmethod make-parser-state ((pd seq-parser-def) &key parent-state)
  ;; Set up cascading continuations.
  (let ((ps (make-instance 'seq-parser-state :def pd :parent-state parent-state)))
    (maplist #'(lambda (npd-rest)
		 (destructuring-bind (npd &rest rest) npd-rest
		   (let ((nps (make-parser-state npd :parent-state ps)))
		     (append1f (nested-parser-state-list ps) nps)
		     (setf (success-continuation nps)
			   #'(lambda (value)
			       (appendf (value ps) value)
			       (pop (nested-parser-state-list ps))
			       (unless rest
				 (complete-parser ps (value ps)))))
		     (setf (failure-continuation nps)
			   #'(lambda (error-message)
			       (fail-parser ps error-message))))))
	     (nested-parser-def-list pd))
    ps))

(defmethod process-sexp ((pd seq-parser-def) (ps seq-parser-state) sexp)
  (declare (ignore pd))
  (let ((ps (first (nested-parser-state-list ps))))
    (process-sexp (def ps) ps sexp)))

;;;; List parser.

(defclass list-parser-def (seq-parser-def) ())
(defclass list-parser-state (seq-parser-state) ())

(defmethod process-sexp ((pd list-parser-def) (ps seq-parser-state) sexp)
  (declare (ignore pd))
  (if (listp sexp)
      (nlet process-list ((rest sexp))
	(let ((nested-sexp (or (first rest) 'end-of-list))
	      (nested-ps (first (nested-parser-state-list ps))))
	  ;(format t "Parsing ~A using ~A~%" nested-sexp nested-ps)
	  (if (null nested-ps)
	      (fail-parser ps "Unexpected token.") ; TODO: what if (eq nested-sexp 'end-of-list)?
	      (handler-case (progn
			      (process-sexp (def nested-ps) nested-ps nested-sexp)
			      (when rest (process-list (rest rest))))
		(unget-sexp () (process-list rest))))))
      (fail-parser ps "List expected.")))

;;;; Optional parser.

(defclass optional-parser-def (sexp-parser-def)
  ((base-parser-def :reader base-parser-def :initarg :base-parser-def)))

(defclass optional-parser-state (sexp-parser-state)
  ((base-parser-state :accessor base-parser-state)))

(defmethod make-parser-state ((pd optional-parser-def) &key parent-state)
  (let* ((ps (make-instance 'optional-parser-state :def pd :parent-state parent-state))
	 (bps (make-parser-state (base-parser-def pd) :parent-state ps)))
    (setf (base-parser-state ps) bps)
    (setf (success-continuation bps) #'(lambda (value) (complete-parser ps value)))
    (setf (failure-continuation bps) #'(lambda (error-message)
					 (declare (ignore error-message))
					 (complete-parser ps nil)
					 (signal 'unget-sexp)))
    ps))

(defmethod process-sexp ((pd optional-parser-def) (ps optional-parser-state) sexp)
  (declare (ignore pd))
  (process-sexp (def (base-parser-state ps)) (base-parser-state ps) sexp))

;;;; Repeating parser.

(defclass repeating-parser-def (sexp-parser-def)
  ((base-parser-def :reader base-parser-def :initarg :base-parser-def)
   (min-occurs :reader min-occurs :initarg :min-occurs)))

(defclass repeating-parser-state (sexp-parser-state)
  ((base-parser-state :accessor base-parser-state)
   (num-occurs :accessor num-occurs :initform 0)
   (value :accessor value :initform nil)))

(defmethod make-parser-state ((pd repeating-parser-def) &key parent-state)
  (let ((ps (make-instance 'repeating-parser-state :def pd :parent-state parent-state)))
    (labels ((initialize-base-parser-state ()
	       (let ((bps (make-parser-state (base-parser-def pd) :parent-state ps)))
		 (setf (base-parser-state ps) bps)
		 (setf (success-continuation bps)
		       #'(lambda (value)
			   (incf (num-occurs ps))
			   (appendf (value ps) value)
			   (initialize-base-parser-state)))
		 (setf (failure-continuation bps)
		       #'(lambda (error-message)
			   (declare (ignore error-message))
			   (if (or (not (slot-boundp pd 'min-occurs))
				   (>= (num-occurs ps) (min-occurs pd)))
			       (progn (complete-parser ps (value ps))
				      (signal 'unget-sexp))
			       (fail-parser ps "Not enough repetitions.")))))))
      (initialize-base-parser-state))
    ps))

(defmethod process-sexp ((pd repeating-parser-def) (ps repeating-parser-state) (sexp (eql 'end-of-list)))
  (declare (ignore pd))
  (complete-parser ps (value ps)))

(defmethod process-sexp ((pd repeating-parser-def) (ps repeating-parser-state) sexp)
  (declare (ignore pd))
  (process-sexp (def (base-parser-state ps)) (base-parser-state ps) sexp))

;;;; One-of parser.

;; TODO: build a dispatch table for all nested defs that are lists with the first subparser being a constant with an expected value

(defclass one-of-parser-def (sexp-parser-def)
  ((nested-parser-def-list :accessor nested-parser-def-list :initarg :nested-parser-def-list :initform nil
			   :documentation "A list of parsers to consider in parallel. The first successful parser completes this parser.")))

(defmethod print-object ((obj one-of-parser-def) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~{~I~A~}" (nested-parser-def-list obj))))

(defclass one-of-parser-state (sexp-parser-state)
  ((nested-parser-state-list :accessor nested-parser-state-list :initarg :nested-parser-state-list :initform nil)
   (matched-p :accessor matched-p :initform nil)))

(defmethod make-parser-state ((pd one-of-parser-def) &key parent-state)
  ;; Set up continuations.
  (let ((ps (make-instance 'one-of-parser-state :def pd :parent-state parent-state)))
    (mapc #'(lambda (npd)
	      (let ((nps (make-parser-state npd :parent-state ps)))
		(append1f (nested-parser-state-list ps) nps)
		(setf (success-continuation nps)
		      #'(lambda (value)
			  ;; First successful nested parser completes this one-of parser.
			  (setf (matched-p ps) t)
			  (complete-parser ps value)))
		(setf (failure-continuation nps)
		      #'(lambda (error-message)
			  ;; Remove this parser from the list of parsers under consideration.
			  (setf (nested-parser-state-list ps) (delete nps (nested-parser-state-list ps)))
			  ;; If this was the last parser, signal an error.
			  (when (null (nested-parser-state-list ps))
			    (fail-parser ps error-message))))))
	(nested-parser-def-list pd))
    ps))

(defmethod process-sexp ((pd one-of-parser-def) (ps one-of-parser-state) sexp)
  (mapc #'(lambda (nps) (unless (matched-p ps) (process-sexp (def nps) nps sexp)))
	(nested-parser-state-list ps)))

;;;; Unordered parser.

;(defclass unordered-parser (sexp-parser)
;  ((nested-parser-list)
;   (ir-remaining-parser-list)))

;;;; S-expression parser compiler.

;;; Data model.

(defun is-sexp-prop (sym)
  ;; TODO: temporary definition
  (is-rule sym))

(defun sexp-prop-name (sym)
  (assert (is-rule sym))
  (gen-symbol nil
	      (subseq (symbol-name sym) 1 (1- (length (symbol-name sym))))))

(deftype sexp-prop () '(satisfies is-sexp-prop))

;;; S-expression syntax definition parser.

(let* ((sexp-syntax-const-pd (make-instance 'constant-parser-def
					    :expected-type 'atom
					    :zexp-tag '(sexp-syntax-const value)))
       (sexp-syntax-prop-pd (make-instance 'constant-parser-def
					   :expected-type 'sexp-prop
					   :zexp-tag '(sexp-syntax-prop name)))
       (sexp-syntax-exp-pd (make-instance 'list-parser-def
					  :zexp-tag 'sexp-syntax-exp
					  :nested-parser-def-list
					  (list (make-instance 'optional-parser-def
							       :base-parser-def
							       (make-instance 'constant-parser-def
									      :expected-type 'sexp-prop
									      :zexp-tag 'name))
						(make-instance 'constant-parser-def
							       :expected-type 'symbol
							       :zexp-tag 'type-class)
						(make-instance 'repeating-parser-def
							       :base-parser-def
							       (make-instance 'ref-parser-def :name 'sexp-syntax-token)))))
       (sexp-syntax-token-pd (make-instance 'one-of-parser-def
					    :nested-parser-def-list
					    (list (make-instance 'ref-parser-def :name 'prop)
						  (make-instance 'ref-parser-def :name 'const)
						  (make-instance 'ref-parser-def :name 'exp))))
       (sexp-language (list (cons 'sexp-syntax-token sexp-syntax-token-pd)
			    (cons 'const sexp-syntax-const-pd)
			    (cons 'prop sexp-syntax-prop-pd)
			    (cons 'exp sexp-syntax-exp-pd))))
  (defun parse-sexp-syntax (syntax)
    (let* ((*sexp-language* sexp-language)
	   (ps (make-parser-state sexp-syntax-token-pd)))
      (setf (success-continuation ps)
	    #'(lambda (value) (return-from parse-sexp-syntax value)))
      (process-sexp sexp-syntax-token-pd ps syntax)
      nil)))

;;; Preprocessor for sexp form definitions.

(defun translate-sexp-form (sexp-form)
  (list* (intern (concatenate 'string "<" (symbol-name (id sexp-form)) ">"))
	 'list
	 (syntax sexp-form)))

(defun preprocess-sexp-language (sexp-form-list)
  (parse-program +sexp-parser-language+
		 (mappend #'parse-sexp-syntax
			  (mapcar #'translate-sexp-form sexp-form-list))))

;;; Data model compiler.

(defclass sexp-model-class ()
  ((name :accessor name :initarg :name :initform nil)
   (prop-list :accessor prop-list :initarg :prop-list :initform nil)
   (nested-class-list :accessor nested-class-list :initarg :nested-class-list :initform nil)))

(defclass sexp-model-prop ()
  ((name :accessor name :initarg :name :initform nil)
   (repeating :accessor repeating :initarg :repeating :initform nil)))

(defgeneric compile-data-model (syntax &key class gen-root-slot context)
  (:documentation "Generates a data model class for a syntax definition. If a prefix is specified, it's used to prefix the class name."))

(defmethod compile-data-model ((syntax sexp-syntax-const) &key class gen-root-slot context)
  (declare (ignore class gen-root-slot context))
  nil)

(defmethod compile-data-model ((syntax sexp-syntax-prop) &key class gen-root-slot context)
  (compile-data-model (make-instance 'sexp-syntax-exp
				     :name (name syntax)
				     :type-class 'sexp)
		      :class class
		      :gen-root-slot gen-root-slot
		      :context context))

(defun make-class-name (base-name &key prefix) ;; TODO: context to prepend to the name...
  (apply #'gen-symbol nil
	 (append prefix (list base-name))))

(defmethod compile-data-model ((syntax sexp-syntax-exp) &key class gen-root-slot context)
  (let* ((class-name (awhen (name syntax) (make-class-name (sexp-prop-name it) :prefix context)))
	 (class (or class (make-instance 'sexp-model-class :name (make-class-name (sexp-prop-name (name syntax))
										 :prefix context))))
	 (context (if class (append context (list class-name)) context)))
    (case (type-class syntax)
      ((+ *) (progn
	       (assert (= 1 (length (body-list syntax))))
	       ;; Composite content within a repeating element calls for a new class.
	       (let ((body (first (body-list syntax))))
		 (when (name body)
		   (append1f (prop-list class) (make-instance 'sexp-model-prop
							      :name (sexp-prop-name (name body))
							      :repeating t)))
		 (when (and (typep body 'sexp-syntax-exp)
			    (member (type-class body) '(* + list seq)))
		   (append1f (nested-class-list class)
			     (let ((class-name (make-class-name (sexp-prop-name (name body)) :prefix context)))
			       (compile-data-model body
						   :class (make-instance 'sexp-model-class :name class-name)
						   :context context)))))))

      (otherwise (progn
		   (when (and gen-root-slot (name syntax))
		     (append1f (prop-list class) (make-instance 'sexp-model-prop :name (sexp-prop-name (name syntax)))))
		   (mapc #'(lambda (s) (compile-data-model s :class class :gen-root-slot t :context context))
			 (body-list syntax)))))
    class))

(defun class->zexp-form-definition (class inherit-list)
  (list* (make-instance 'zexp-form-definition
			:id (name class)
			:inherit-list inherit-list
			:prop-list (mapcar #'(lambda (prop)
					       (make-instance 'zexp-prop-definition
							      :id (name prop)
							      :repeating-p (repeating prop)))
					   (prop-list class)))
	 (mappend #'class->zexp-form-definition (nested-class-list class))))

(defun compile-zexp-form-definitions (syntax-list sexp-defs)
  (let* ((compiled-sexp-defs (mapcar #'compile-data-model syntax-list))
	 (translated-sexp-defs (mappend #'class->zexp-form-definition
					compiled-sexp-defs
					(mapcar #'inherit-list sexp-defs))))
    (dolist (sexp-def translated-sexp-defs)
      (dolist (prop (prop-list sexp-def))
	(post-process prop))
      (post-process sexp-def))
    translated-sexp-defs))

;;; Parser definition compiler.

(defgeneric compile-parser-def (syntax &key context new-class in-collection))

(defmethod compile-parser-def ((syntax sexp-syntax-const) &key context new-class in-collection)
  (declare (ignore context new-class in-collection))
  `(make-instance 'constant-parser-def
    ,@(awhen (value syntax) `(:expected-value ',it :ignore-value t))))

(defmethod compile-parser-def ((syntax sexp-syntax-prop) &key context (new-class t) in-collection)
  (compile-parser-def (make-instance 'sexp-syntax-exp
				     :name (name syntax)
				     :type-class 'sexp)
		      :context context
		      :new-class new-class
		      :in-collection in-collection))

(defmethod compile-parser-def ((syntax sexp-syntax-exp) &key context (new-class t) in-collection)
  (let* ((class-tag (when (and new-class (name syntax))
		      (list (make-class-name (sexp-prop-name (name syntax)) :prefix context))))
	 (prop-tag (when (and (name syntax)
			      (or in-collection (not new-class)))
		     (list (sexp-prop-name (name syntax)))))
	 (zexp-tag (aand (append class-tag prop-tag) `(:zexp-tag ',it)))
	 (context (if (and new-class (name syntax))
		      (append context (list (sexp-prop-name (name syntax))))
		      context)))
    (case (type-class syntax)
      ((list seq) `(make-instance ',(if (eq (type-class syntax) 'list) 'list-parser-def 'seq-parser-def) ,@zexp-tag
		    :nested-parser-def-list (list ,@(mapcar #'(lambda (body)
								(compile-parser-def body
										    :context context
										    :new-class nil))
							    (body-list syntax)))))
      (? `(make-instance 'optional-parser-def ,@zexp-tag
	   :base-parser-def ,(compile-parser-def (first (body-list syntax)) :context context :new-class nil)))
      ;; TODO: if (> length 1), implement an implicit seq.
      ((* +) `(make-instance 'repeating-parser-def ,@zexp-tag
	       ,@(when (eq (type-class syntax) '+) '(:min-occurs 1))
	       :base-parser-def ,(let ((body (first (body-list syntax))))
				      (compile-parser-def body
							  :context context
							  :new-class (and (typep body 'sexp-syntax-exp)
									  (member (type-class body) '(* + list seq)))
							  :in-collection t))))
      ;; TODO: this whole one-of block is temporary
      (one-of `(make-instance 'one-of-parser-def ,@zexp-tag
		:nested-parser-def-list (list ,@(mapcar #'(lambda (body)
							    (compile-parser-def body
										:context context
										:new-class nil))
							(body-list syntax)))))
      (otherwise `(make-instance 'ref-parser-def :name ',(type-class syntax) ,@zexp-tag)))))

(defun compile-sexp-language-parser (lang-name syntax-list)
  (let ((fn-name (gen-symbol nil :parse-sexp lang-name)))
    (values
     `(let*
       (,@(mapcar #'(lambda (syntax)
		      (list (gen-symbol nil (sexp-prop-name (name syntax)) :pd)
			    (compile-parser-def syntax)))
		  syntax-list)
	(sexp-top-level-pd (make-instance 'one-of-parser-def
					  :nested-parser-def-list
					  (list ,@(mapcar #'(lambda (syntax)
							      `(make-instance 'ref-parser-def
								:name ',(sexp-prop-name (name syntax))))
							  syntax-list)
						(make-instance 'ref-parser-def :name 'number)
						(make-instance 'ref-parser-def :name 'string)
						(make-instance 'ref-parser-def :name 'symbol))))
	(sexp-language
	 (list (cons 'sexp sexp-top-level-pd)
	       (cons 'number (make-instance 'constant-parser-def :expected-type 'number))
	       (cons 'string (make-instance 'constant-parser-def :expected-type 'string))
	       (cons 'symbol (make-instance 'constant-parser-def :expected-type 'atom))
	       ,@(mapcar #'(lambda (syntax)
			     (let ((name (sexp-prop-name (name syntax))))
			       `(cons ',name ,(gen-symbol nil name :pd))))
			 syntax-list))))
       (defun ,fn-name (sexp)
	 (let* ((*sexp-language* sexp-language)
		(ps (make-parser-state sexp-top-level-pd)))
	   (setf (success-continuation ps)
		 #'(lambda (value) (return-from ,fn-name (first value))))
	   (process-sexp sexp-top-level-pd ps sexp)
	   nil #| TODO: report error |#)))
     fn-name)))
