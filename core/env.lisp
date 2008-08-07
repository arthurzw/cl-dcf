(in-package :com.google.catharsis)

(defvar *env* nil)

(defclass definition ()
  ((id :accessor id :initarg :id :initform nil)
   (parent :accessor parent :initarg :parent :initform nil)
   (env :accessor env :initarg :env :initform *env*)
   (local-env :accessor local-env :initarg :localenv :initform nil)
   (normalized-body :accessor normalized-body :initarg :normalized-body :initform nil))) ; TODO: add file name, line number, parent definition.

(defclass env ()
  ((id :accessor id :initarg :id :initform nil)
   (parent :accessor parent :initarg :parent :initform nil)
   (children :accessor children :initarg :children :initform nil)
   (sym-table :accessor sym-table :initarg :sym-table :initform (make-hash-table))))

(defun make-env (&optional initial-env)
  (let ((new-env (make-instance 'env)))
    (when initial-env
      (walk-env #'(lambda (def env) (declare (ignore env)) (store-def def new-env))
		initial-env
		nil))
    new-env))

(defun make-local-env (&key id (parent *env*))
  (let ((new-env (make-instance 'env :id id :parent parent)))
    (when parent (append1f (children parent) new-env))
    new-env))

(defun env->sexp (env)
  `(env ,@(awhen (parent env) `((parent ,(env->sexp it))))
    (sym-table ,(hashtable->list (sym-table env)))))

(defun env->sexp-top-down (env)
  `(env
    ,(id env)
    ,@(hashtable->list (sym-table env))
    ,@(mapcar #'env->sexp-top-down (children env))))

(defun store-def (def &optional (env *env*))
  (let ((id (id def))
	(sym-table (sym-table env)))
    (unless id (error "ID not specified."))
    (if (gethash id sym-table)
	(error "Symbol ~A already defined in this scope." id)
	(setf (gethash id sym-table) def))
    env))

(defun lookup-def (id &optional (env *env*) self-ref-to-avoid)
  (when env (or (let ((candidate (gethash id (sym-table env))))
		  (unless (eq candidate self-ref-to-avoid)
		    candidate))
		(lookup-def id (parent env)))))

(defun lookup-def-by-type (type &optional (env *env*))
  (when env (or (block nil
		  (maphash #'(lambda (id def)
			       (declare (ignore id))
			       (when (typep def type) (return def)))
			   (sym-table env)))
		(lookup-def-by-type type (parent env)))))

(defun walk-env (fn &optional (env *env*) (recursive-p t))
  "Calls fn on all definition in the environment and all its nested environments. The function fn is called with (def env)."
  (maphash #'(lambda (id def)
	       (when (eq id (id def))
		 (funcall fn def env)))
	   (sym-table env))
  (when recursive-p
    (mapc (curry #'walk-env fn) (children env))))

(defun find-all-defs-of-type (type &optional (env *env*) (recursive-p t))
  "Retrieves all definitions that match the supplied type."
  (let ((result nil))
    (walk-env #'(lambda (def env)
		  (declare (ignore env))
		  (when (typep def type)
		    (append1f result def)))
	      env recursive-p)
    result))

(defpackage :com.google.catharsis.temp-syms
  (:documentation "Package used for temporary symbols generated during z-expression parsing."))

(defvar *temp-sym-counter* 0)
(defun gen-temp-sym () (gen-symbol :com.google.catharsis.temp-syms :t (incf *temp-sym-counter*)))

(defun generate-missing-ids (&optional (env *env*))
  "Generates IDs for all definitions in the environment. Ideally, the ID picked is the name of the primary class of the definition. If that creates a conflict, sequential IDs are used, starting with a trailing '-1'."
  (mapc #'generate-missing-ids (children env))
  (let ((renamed-defs nil))
    (maphash #'(lambda (id def)
		 (when (eq (symbol-package id)
			   (find-package :com.google.catharsis.temp-syms))
		   (setf (id def)
			 (let ((primary-class-name (type-of def)))
			   (nlet rec ((seq 1) (candidate-name primary-class-name))
			     (if (or (lookup-def candidate-name env)
				     (member candidate-name renamed-defs :key (compose #'id #'cdr)))
				 (rec (1+ seq)
				      (gen-symbol (symbol-package primary-class-name)
						  primary-class-name
						  seq))
				 candidate-name))))
		   (push (cons id def) renamed-defs)))
	     (sym-table env))
    (mapcar #'(lambda (id-def)
		(destructuring-bind (id . def) id-def
		  (store-def def env)
		  id))
	    renamed-defs)))

(defun env->list (&optional (env *env*))
  (nlet rec ((env env) (acc nil))
    (if env
	(progn
	  (maphash #'(lambda (k v)
		       (setf acc (adjoin (list k v) acc :key #'first)))
		   (sym-table env))
	  (rec (parent env) acc))
	(nreverse acc))))

(defmacro with-local-env (&body body)
  `(let ((*env* (make-local-env)))
    ,@body))
