(in-package :com.google.hello-world)

(defgeneric compile-def (def))

(defmethod compile-def ((person person-definition))
  `((cpp:call lower-case::printf
     (cpp:string-literal "  Person %s: name - %s; phone-number - %s\\n")
     (cpp:string-literal ,(symbol-name (id person)))
     (cpp:string-literal ,(name person))
     (cpp:string-literal ,(phone-number person)))))

(defmethod compile-def ((group group-definition))
  `((cpp:call lower-case::printf
     (cpp:string-literal "Group %s:\\n")
     (cpp:string-literal ,(symbol-name (id group))))
    ,@(mappend #'compile-def (element-list group))))

(defun compile-program (group-defs)
  `((cpp:// "This is a generated Hello World program.")
    (cpp:_)
    (cpp:std-include "stdio.h")
    (cpp:class :hello-world
     (cpp:public
      (cpp:method :print
		  (cpp:call lower-case::printf (cpp:string-literal "Hello, world!\\n"))
		  ,@(mappend #'compile-def group-defs))))))

(test "compile-program"
      nil
      (prog1 nil
	(print
	 (compile-program
	  (remove-if-not #'(lambda (def) (typep def 'group-definition))
			 (first *parsed-program*))))))

(cpp:generate-file (compile-program
		    (remove-if-not #'(lambda (def) (typep def 'group-definition))
				   (first *parsed-program*)))
		   "hello.cpp")
