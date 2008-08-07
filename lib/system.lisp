(in-package :com.google.common)


(defun get-env (var)
  "Retrieves the specified enviroment variable's value or nil if not found.
   The parameter should be a keyword, e.g. :home."
  (cdr (assoc var extensions:*environment-list*)))

(export 'get-env)


(defun get-pwd ()
  "Retrieves the current working directory."
  (truename *default-pathname-defaults*))

(export 'get-pwd)
