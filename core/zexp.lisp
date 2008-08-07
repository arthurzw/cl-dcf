(in-package :com.google.catharsis)


(defun tag-p (sym)
  (and (not (keywordp sym))
       (atom sym)
       (not (functionp sym))))


(defun zexp-tag-name-rest (zexp)
  "Parses the input z-expression into a triple: (tag name rest).
   If any of the three are not specified, it's set to nil."
  (let* ((first (first zexp))
	 (second (second zexp))
	 (tag (when (tag-p first)
		first))
	 (name (when (and tag (or (keywordp second)
				  (and (symbolp second) (casing-package-p second))))
		 second))
	 (rest (cond ((and tag name) (cddr zexp))
		     ((or tag name) (cdr zexp))
		     (t zexp))))
    (list tag name rest)))


(defun get-zexp-tag (zexp)
  (when (listp zexp)
    (let ((first (first zexp)))
      (when (tag-p first)
	first))))

(export 'get-zexp-tag)


(defun get-zexp-name (zexp)
  (second (zexp-tag-name-rest zexp)))

(export 'get-zexp-name)


(defun get-zexp-canonical-name (zexp)
  (aif (get-zexp-name zexp)
       it
       (gen-symbol :keyword (get-zexp-tag zexp))))

(export 'get-zexp-canonical-name)


(defun get-zexp-value (zexp)
  (let ((potential-value (first (third (zexp-tag-name-rest zexp)))))
    (if (or (atom potential-value)
	    (eq (first potential-value) 'quote))
	potential-value
	(second (spath zexp 'catharsis::value)))))

(export 'get-zexp-value)

(test "get-zexp-value"
      '(val1 val2)
      (list
       (get-zexp-value '(tag :name val1))
       (get-zexp-value '(tag :name (catharsis::value val2)))))


(defun get-zexp-head (zexp)
  (remove-if #'null (mapcar #'(lambda (fn) (funcall fn zexp))
			    `(,#'get-zexp-tag ,#'get-zexp-name ,#'get-zexp-value))))

(export 'get-zexp-head)

(test "get-zexp-head"
      '(test-name :test-name '(test-value))
      (get-zexp-head '(test-name :test-name '(test-value) (junk))))


(defun get-zexp-rest (zexp)
  (if (get-zexp-value zexp)
      (rest (third (zexp-tag-name-rest zexp)))
      (third (zexp-tag-name-rest zexp))))

(export 'get-zexp-rest)


(defun get-zexp-value-rest (zexp)
  (third (zexp-tag-name-rest zexp)))

(export 'get-zexp-value-rest)


(defun append-zexp (zexp prop)
  (if prop
      (append zexp (list prop))
      zexp))

(export 'append-zexp)


(defun is-zexp-of-type (zexp tag)
  (and (listp zexp)
       (eq (get-zexp-tag zexp) tag)))

(export 'is-zexp-of-type)


(defun zexp-remove-properties (zexp prop-tag)
  (mappend #'(lambda (prop)
	       (unless (is-zexp-of-type prop prop-tag)
		 (list prop)))
	   zexp))

(export 'zexp-remove-properties)


(defun read-zexp-file (filename)
  (let* ((file-contents `(file ,filename))
	 (*package* *package*))
    (with-open-file (src filename)
      (loop for sexp = (read src nil)
	    until (null sexp)
	    do (destructuring-bind (tag name rest)
		   (zexp-tag-name-rest sexp) ; TODO(arthurz): zexp-tag-name-rest is deprecated...
		 (declare (ignore name rest))
		 (if (eq tag 'in-package)
		     (setf *package* (or (find-package (second sexp))
				      (error "Unknown package ~S." (second sexp))))
		     (setf file-contents (append-zexp file-contents sexp))))))
    file-contents))

(export 'read-zexp-file)
