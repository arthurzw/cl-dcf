(in-package :com.google.catharsis)

;;;; Language definition class.

(defclass language ()
  ((dispatch-table :accessor dispatch-table :initarg :dispatch-table :initform nil
		   :documentation "Provides a mapping from the top of the definition stack to corresponding class names.")
   (macro-dispatch-table :accessor macro-dispatch-table :initarg :macro-dispatch-table :initform (make-hash-table))
   (sexp-parser-fn :reader sexp-parser-fn :initarg :sexp-parser-fn :initform nil)))

(defvar *language* nil "Current language.")
(defvar *def-stack* nil "Definition stack.")
(defvar *sexp-expanded* nil)
(defvar *sexp->obj-map* nil "Mapping of s-expression objects to corresponding objects. Used to communicate between phases I and II.")

(defun lookup-class (stack)
  "Determines which class the element on top of the specified stack parses into, taking context into account."
  (lookup-prefix-in-trie stack (dispatch-table *language*)))

;;;; Generic functions invoked by all parsers to customize behavior.

(defgeneric verify-def (obj)
  (:documentation "Verifies the integrity of the object."))

(defgeneric post-process (obj)
  (:documentation "Post-processes the object after the entire input has been parsed. This is a good place to do semantic analysis."))

;;;; Generic functions used internally by parsers.

(defgeneric parse-zexp-property-by-tag (obj tag body)
  (:documentation "Parses a z-expression property by its tag."))

(defgeneric parse-zexp-property-by-type (obj def)
  (:documentation "Parses a z-expression property by its type."))

(defgeneric set-defaults (obj)
  (:documentation "Sets the default values of all unspecified properties."))

(defmethod parse-zexp-property-by-tag (obj tag body)
  "Base implementation that returns failure."
  :not-found)

(defmethod parse-zexp-property-by-type (obj def)
  "Base implementation that returns failure."
  :not-found)

(defmethod verify-def (obj) (when (next-method-p) (call-next-method)))
(defmethod post-process (obj) (when (next-method-p) (call-next-method)))
(defmethod set-defaults (obj) (when (next-method-p) (call-next-method)))

;;;; Internal parser functions.

(defun parse-zexp-prop-value-phase-1 (obj value)
  "See parse-zexp-prop-value-phase-2. In phase 1, we're only concerned about parsing Z-expression type instantiations."
  (when (and (consp value)
	     (not (eq (first value) 'quote)))
    (let ((*env* (make-local-env :id (id obj))))
      (parse-zexp-phase-1 value :parent obj))))

(defun parse-zexp-prop-value-phase-2 (obj value)
  "Parses a property value and interprets it. Quoted values and other basic types are preserved. Z-expression type instantiations are evaluated, and references are resolved."
  (acond
   ;; NIL
   ((null value) nil)
   ;; Quoted values.
   ((unquote value) it)
   ;; Numbers.
   ((numberp value) value)
   ;; Strings.
   ((stringp value) value)
   ;; Keywords.
   ((keywordp value) value)
   ;; Symbols (references).
   ((symbolp value) (or (lookup-def value *env* obj)
			(error "Entity referenced by ~A not found." value)))
   ;; Z-expressions (nested definitions).
   (t (gethash value *sexp->obj-map*))))

(defun normalize-zexp (zexp)
  "Normalizes zexp syntax to (id tag . body). The id is generated using (gen-temp-sym) if it's not specified."
  (destructuring-bind (tag . body) zexp
    (let ((tag-name (symbol-name tag)))
      (cond ((and (> (length tag-name) 4)
		  (equal (subseq tag-name 0 4) "DEF-")) ; (def-foo id . body) syntax.
	     (destructuring-bind (id . body) body
	       (list* id
		      (gen-symbol (symbol-package tag)
				  (subseq tag-name 4))
		      body)))

	    ((eq tag 'def) ; (def id (foo . body)) syntax.
	     (destructuring-bind (id (tag . body)) body
	       (list* id tag body)))

	    (t ; (foo . body) syntax (anonymous definition).
	     (list* (gen-temp-sym) tag body))))))

(defun macroexpand-zexp (zexp)
  "If the zexp is a macro invocation, returns its expansion. Otherwise, returns the argument."
  (case (gethash (first zexp) (macro-dispatch-table *language*))
    ;; TODO: deal with 'catharsis:end-of-list...
    (:sexp (if *sexp-expanded* zexp (values (funcall (sexp-parser-fn *language*) zexp) t)))
    (t zexp)))

(defun parse-zexp-phase-1 (zexp &key parent)
  "Phase 1 of z-expression parsing: create a skeleton object and all nested objects (recursively)."
  (multiple-value-bind (macroexpanded-zexp sexp-expanded)
      (macroexpand-zexp zexp)
    (destructuring-bind (id tag . body) (normalize-zexp macroexpanded-zexp)
      (let* ((*sexp-expanded* (or *sexp-expanded* sexp-expanded))
	     (*def-stack* (cons tag *def-stack*))
	     (class (lookup-class *def-stack*)))
	(unless class
	  (error "Unexpected expression ~S." zexp))
	(let ((obj (make-instance class :id id :parent parent)))
	  (setf (gethash zexp *sexp->obj-map*) obj)
	  (let ((*env* (or (local-env obj) *env*))) ; Use a local environment if one is defined.
	    (setf (normalized-body obj)
		  (mapcar
		   #'(lambda (exp)
		       (if (listp exp)
			   (destructuring-bind (prop-id prop-tag . prop-body)
			       (normalize-zexp exp)
			     (declare (ignore prop-id))
			     (if (find prop-tag (prop-tags obj))
				 (progn
				   (parse-zexp-prop-value-phase-1 obj (first prop-body))
				   (list* nil prop-tag prop-body))
				 (let ((child-obj (parse-zexp-phase-1 exp :parent obj)))
				   (list* (id child-obj) prop-tag prop-body))))
			   exp))
		   body)))
	  (store-def obj)
	  obj)))))

(defun parse-zexp-phase-2 (obj)
  "Phase 2 of z-expression parsing: property parsing and validation."
  (let* ((*env* (or (local-env obj) (env obj)))) ; Use a local environment if one is defined.
    (mapc #'(lambda (normalized-exp)
	      (if (listp normalized-exp)
		  ;; Parse a property.
		  (destructuring-bind (prop-id prop-tag . prop-body) normalized-exp
		    (if prop-id
			(when (eq (parse-zexp-property-by-type obj (lookup-def prop-id))
				  :not-found)
			  (error "Unrecognized property ~A ~A." prop-id (cons prop-tag prop-body)))) ; TODO: use conditions.
			(parse-zexp-property-by-tag obj prop-tag prop-body))
		  ;; Parse an identifier (a reference).
		  (aif (lookup-def normalized-exp *env* obj)
		       (when (eq (parse-zexp-property-by-type obj it) :not-found)
			 (error "Reference to ~A is not allowed in this context." normalized-exp))
		       (error "~A: invalid reference." normalized-exp))))
	  (normalized-body obj)))
  obj)

(defun parse-program (lang prog &key default-env)
  (let* ((*language* lang)
	 (*sexp->obj-map* (make-hash-table))
	 (*env* (make-env default-env))
	 (top-level-defs (mapcar #'parse-zexp-phase-1 prog))
	 (all-defs nil))
    (generate-missing-ids *env*)
    ;; Post-processing.
    (mapc #'(lambda (fn)
	      (walk-env #'(lambda (def env)
			    (declare (ignore env))
			    (unless (eq (env def) default-env)
			      (funcall fn def)))))
	  (list #'parse-zexp-phase-2
		#'set-defaults
		#'verify-def
		#'post-process
		#'(lambda (def) (append1f all-defs def))))
    ;(pprint (env->sexp-top-down *env*))
    (values top-level-defs all-defs *env*)))

(defun inspect-parse-tree (lang prog)
  (mapcar (rcurry #'object->sexp :suppress-types '(env) :suppress-properties '(normalized-body))
	  (parse-program lang prog)))
