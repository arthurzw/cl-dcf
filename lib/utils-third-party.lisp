(in-package :com.google.common)


(defmacro with-unique-names ((&rest bindings) &body body)
  `(let ,(mapcar #'(lambda (binding)
                     (destructuring-bind (var prefix)
			 (if (consp binding) binding (list binding binding))
                       `(,var (gensym ,(string prefix)))))
                 bindings)
    ,@body))

(export 'with-unique-names)


;;;; From Paul Graham, "ANSI Common Lisp" and "On Lisp".

(defun compose (&rest fns)
  (destructuring-bind (fn1 . rest) (reverse fns)
    #'(lambda (&rest args)
        (reduce #'(lambda (v f) (funcall f v))
                rest
                :initial-value (apply fn1 args)))))

(export 'compose)


;;; Currying.

(defun curry (fn &rest args)
  #'(lambda (&rest args2)
      (apply fn (append args args2))))

(defun rcurry (fn &rest args)
  #'(lambda (&rest args2)
      (apply fn (append args2 args))))

(export 'curry)
(export 'rcurry)


;;; Anaphoric macros.

(export 'it)


(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
    (if it ,then-form ,else-form)))

(export 'aif)


(defmacro awhen (test-form &body body)
  `(aif ,test-form
    (progn ,@body)))

(export 'awhen)


(defmacro awhile (expr &body body)
  `(do ((it ,expr ,expr))
    ((not it))
    ,@body))

(export 'awhile)


(defmacro aand (&rest args)
  (cond ((null args) t)
        ((null (cdr args)) (car args))
        (t `(aif ,(car args) (aand ,@(cdr args))))))

(export 'aand)


(defmacro acond (&rest clauses)
  (if (null clauses)
      nil
      (let ((cl1 (car clauses))
            (sym (gensym)))
        `(let ((,sym ,(car cl1)))
           (if ,sym
               (let ((it ,sym)) (declare (ignorable it)) ,@(cdr cl1))
               (acond ,@(cdr clauses)))))))

(export 'acond)


;;;

(defun mappend (fn &rest lsts)
  "Applies fn to every member of the lists, and appends the results together."
  (apply #'append (apply #'mapcar fn lsts)))

(export 'mappend)

;; (test "mappend-1"
;;       '(11 22 33)
;;       (mappend #'(lambda (a b) (list (+ a b))) '(1 2 3) '(10 20 30)))

;; (test "mappend-2"
;;       '(1 -1 2 -2 3 -3)
;;       (mappend #'(lambda (x) (list x (- x))) '(1 2 3)))


(define-modify-macro appendf (val)
  (lambda (lst val) (append lst val)))

(export 'appendf)


(define-modify-macro append1f (val)
  (lambda (lst val) (append lst (list val))))

(export 'append1f)
