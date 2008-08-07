(in-package :com.google.hello-world)

(compile-language hello-world

  (def-zexp-form person
    (def-prop name (type 'string))
    (def-prop phone-number (type 'string)))

  (def-zexp-form group
    (def-prop element (repeating) (type person) (parse-by-type))))

;;; Test case.

(defconstant +test-program+
  (drop 2 (read-zexp-file #p"test.hello")))

(defvar *parsed-program* (multiple-value-list (parse-program +hello-world-language+ +test-program+)))
