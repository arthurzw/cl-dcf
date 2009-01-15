(in-package :com.google.common)


(defun string+ (&rest str-list)
  (apply #'concatenate 'string str-list))

(export 'string+)

(test "string+"
      "abc"
      (string+ "a" "b" "c"))


(defun concatenate-strings-with-separator (separator &rest strings)
  "Appends the supplied strings together, adding a separator in-between."
  (nlet rec ((acc "") (strings strings))
    (cond ((null strings) acc)
	  ((null (rest strings)) (string+ acc (first strings)))
	  (t (rec (string+ acc (first strings) separator)
            (rest strings))))))

(export 'concatenate-strings-with-separator)


(defun concatenate-strings-with-spaces (&rest strings)
  (apply #'concatenate-strings-with-separator " " strings))

(export 'concatenate-strings-with-spaces)

(test "concatenate-strings-with-spaces"
      '("" "str1" "str1 str2 str3")
      (mapcar (curry #'apply #'concatenate-strings-with-spaces)
	      '(nil
          ("str1")
          ("str1" "str2" "str3"))))

(defun string-case-fun (keyform cases)
  `(cond ,@(mapcar #'(lambda (case)
                       (destructuring-bind (form body) case
                         (list
                          (if (eq form 'otherwise)
                              t
                              `(string= ,keyform ,form))
                          body)))
                   cases)))

(test "string-case-fun"
      '(cond
        ((string= str-keyform case1) value1)
        ((string= str-keyform case2) value2)
        (t value3))
      (string-case-fun 'str-keyform
                       '((case1 value1)
                         (case2 value2)
                         (otherwise value3))))

(defmacro string-case (keyform &body cases)
  (string-case-fun keyform cases))

(export 'string-case)

(defun symbol-name-k (s)
  "Returns the name of the symbol s, including the leading ':' if s is a keyword."
  (let ((sym-name (symbol-name s)))
    (if (keywordp s)
        (string+ ":" sym-name)
        sym-name)))

(export 'symbol-name-k)

(test "symbol-name-k"
      '("ABC" ":DEF")
      (list (symbol-name-k 'abc)
	    (symbol-name-k :def)))


(defun make-keyword (str-or-sym)
  "Creates a keyword out of str-or-sym, which can be either a string or a symbol."
  (intern (etypecase str-or-sym
	    (string (string-upcase str-or-sym))
	    (symbol (symbol-name str-or-sym)))
	  :keyword))

(export 'make-keyword)

(test "make-keyword"
      '(:abc :def)
      (mapcar #'make-keyword '("abc" def)))


(defun last-char (str)
  (let ((len (length str)))
    (if (zerop len)
	nil
	(char str (1- len)))))

(export 'last-char)

(test "last-char"
      '(#\c nil)
      (list (last-char "abc")
	    (last-char "")))


(defun gen-symbol (package &rest params)
  "Generates a new symbol from a list of name pieces. Each piece can either
   be an existing symbol or a string. Dashes are inserted in-between pieces."
  (intern (string-upcase
	   (nlet rec ((acc nil) (rest (mapcar #'(lambda (param)
						  (cond ((stringp param) param)
							((characterp param) (string param))
							((symbolp param) (symbol-name param))
							(t (write-to-string param))))
					      (remove nil params))))
	     (if rest
		 (if (null acc)
		     (rec (first rest) (rest rest))
		     (rec (string+ acc "-" (first rest)) (rest rest)))
		 acc)))
	  (or package *package*)))

(export 'gen-symbol)

(test "gen-symbol"
      'first-second-3
      (gen-symbol nil "first" :second 3 nil))


(defun gen-special-symbol (package delimiter &rest params)
  (intern (string+ (symbol-name delimiter)
		   (symbol-name (apply #'gen-symbol package params))
		   (symbol-name delimiter))
          (or package *package*)))

(export 'gen-special-symbol)

(test "gen-special-symbol"
      '+constant+
      (gen-special-symbol nil :+ :constant))


(defun rename-symbol (sym rename-fn)
  "Generates a new symbol in the same package as sym, using rename-fn to
   compute a new name."
  (intern (funcall rename-fn (symbol-name sym))
	  (symbol-package sym)))

(export 'rename-symbol)

(test "rename-symbol"
      'new-name
      (rename-symbol 'name #'(lambda (x) (string+ "NEW-" x))))


(defun symbol->keyword (sym)
  "Generates a keyword with the same name as the given symbol."
  (intern (symbol-name sym) :keyword))

(export 'symbol->keyword)

(test "symbol->keyword"
      :my-keyword
      (symbol->keyword 'my-keyword))


(defvar *interned-gensym-counter* 0)

(defun interned-gensym (&optional (prefix-sym "IG") (package :keyword))
  "Generates an interned gensym. The default package is keyword, for use in UL code generation."
  (gen-symbol package prefix-sym (incf *interned-gensym-counter*)))

(export 'interned-gensym)
(export '*interned-gensym-counter*)

(test "interned-gensym"
      :ig-11
      (let ((*interned-gensym-counter* 10))
	(interned-gensym)))


(defun get-cmdline-arg (arg-name cmd-line)
  "Look for ... :arg-name arg-val, returning arg-val"
  (second (member (concatenate 'string ":" arg-name)
		  cmd-line :test #'equal)))

(export 'get-cmdline-arg)

(test "get-cmdline-arg"
      "bar"
      (get-cmdline-arg "foo"
                       '("prog" ":arg1" "val1" ":foo" "bar" ":arg3" "val3")))

(defun get-cmdline-arg-list (arg-name cmd-line)
  "Look for ... :arg-name arg-val1 arg-val2, returning arg-val1 arg-val2"
  (nlet rec ((list (rest (member (concatenate 'string ":" arg-name)
				 cmd-line :test #'equal)))
	     (acc nil))
    (if (or (null list) (char= (char (first list) 0) #\:))
	(nreverse acc)
	(rec (rest list) (cons (first list) acc)))))

(export 'get-cmdline-arg-list)

(test "get-cmdline-arg-list"
      '("bar" "baz")
      (get-cmdline-arg-list "foo"
			    '("prog" ":arg1" "val1" ":foo" "bar" "baz" ":arg3" "val3")))
