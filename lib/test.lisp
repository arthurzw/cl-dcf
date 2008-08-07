(in-package :com.google.common)

#|

This module provides a unit test infrastructure.

Tests are defined as in:

; (test :cdr-1          ; test name
;       2               ; expected value
;       (cdr '(1 . 2))) ; test expression

 This library is designed with the following usage patterns in mind:

 * Interactive single-test execution in SLIME. The idea is to put tests directly
   after routines or group of routines they test. Then, after evaluating appropriate
   definitions, the user evaluates the test expression (using C-M-x). This gives
   immediate feedback.

 * Individual package tests. When the user loads and compiles a whole file (using C-c C-k),
   all tests in the package are evaluated silently. The last statement in the file
   (test:print-package-report) prints a summary at the end of REPL output.

 * All unit tests run together when the system is loaded. The .asd definition of
   the system loads (at the end) a file that contains a call to (test:print-global-report),
   which prints a summary of all tests across all packages. This provides a one-glance view
   of whether everything is functioning correctly.

 * Arbitrary REPL invocation of any test or group of tests (by package):
   (run-tests :package ... :name ...).

|#


(defvar *test-table* (make-hash-table :test #'equal)
  "A table of all registered tests. It's a hash table mapping (package name) to test.")


(defstruct test package name expected-value fn)


(defun clear-tests()
  "Removes all currently defined tests."
  (setf *test-table*
	(make-hash-table :test #'equal)))

(export 'clear-tests)


(defun print-tests()
  (maphash #'(lambda (k v)
	       (declare (ignore k))
	       (format t "~A ~A~%" (test-package v) (test-name v)))
	   *test-table*))


(defun run-tests (&key package name)
  "Runs all tests that match the input predicates (package and name)."
  (when (and package (symbolp package)) (setf package (symbol-name package)))
  (when (and name (symbolp name)) (setf name (symbol-name name)))
  (flet ((run-test (test)
	   (let ((test-result (funcall (test-fn test))))
	     (if (equalp test-result (test-expected-value test))
		 t
		 (values nil
			 (format nil "Expected: ~S~%  Actual: ~S"
				 (test-expected-value test)
				 test-result))))))
    (if (and package name)
	(awhen (gethash (list package name) *test-table*)
	  (format t "~&Running test ~A..." (test-name it))
	  (multiple-value-bind (succeeded failure-message)
	      (run-test it)
	    (if succeeded
		(format t " OK.~%")
		(format t " FAILED!~%  ~A~%" failure-message))
	    succeeded))
	(let ((num-run 0)
	      (num-succeeded 0)
	      (failed-tests nil))
	  (maphash #'(lambda (- test)
		       (when (and (or (not package)
				      (eql package (test-package test)))
				  (or (not name)
				      (eql name (test-name test))))
			 (incf num-run)
			 (multiple-value-bind (succeeded failure-message)
			     (run-test test)
			   (if succeeded
			       (progn (format t ".")
				      (incf num-succeeded))
			       (progn (format t "X")
				      (setf failed-tests
					    (cons (format nil "Test ~A failed.~%  ~A~%"
							  (test-name test)
							  failure-message)
						  failed-tests)))))
			 (when (= (mod num-run 100) 0)
			   (format t "~%"))))
		   *test-table*)
	  (format t "~%~d/~d tests succeeded.~%" num-succeeded num-run)
	  (format t "~{~A~}" (nreverse failed-tests))
	  (= num-run num-succeeded)))))

(export 'run-tests)


(defun run-package-tests () (run-tests :package (package-name *package*)))
(export 'run-package-tests)


(defmacro test (name expected-value test-expr)
  `(progn

    ;; Store the test in the test table.
    (setf (gethash (list (package-name *package*) ,name) *test-table*)
     (make-test
      :package (package-name *package*)
      :name ,name
      :expected-value ,expected-value
      :fn #'(lambda () (let ((*package* ,*package*)) ,test-expr))))

    ;; If this test definition is evaluated interactively, run it.
    (eval-when (:execute) (run-tests :package (package-name *package*) :name ,name))))

(export 'test)

(test "cdr-1" 2 (cdr '(1 . 2)))


(defmacro with-defuns (fun-list &body body)
  "Evaluates the body with functions from fun-list defined as global functions.
   The specified global functions are returned to their prior bindings (if any) before returning.
   The syntax is identical to flet.
   This is convenient for defining local test functions that must be accessible using (symbol-function)."
  `(let ((old-bindings (list ,@(mapcar #'(lambda (fun)
					   `(and (fboundp ',(first fun))
					     (symbol-function ',(first fun))))
				       fun-list))))
    ,@(mapcar #'(lambda (fun)
		  (destructuring-bind (name lambda-list &rest body) fun
		    `(setf (symbol-function ',name)
		      #'(lambda ,lambda-list ,@body))))
	      fun-list)
    (unwind-protect (progn ,@body)
      (mapc #'(lambda (name old-binding)
		(if old-binding
		    (setf (symbol-function name) old-binding)
		    (fmakunbound name)))
	    ',(mapcar #'first fun-list)
	    old-bindings))))

(export 'with-defuns)

(test "with-defuns"
      '(:foo 2)
      (with-defuns ((foo () :foo)
		    (bar (arg) arg))
	(list (funcall (symbol-function 'foo))
	      (funcall 'bar 2))))
