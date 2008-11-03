(in-package :com.google.catharsis.java)

(def-grammar java

  (rule <//> ::= "//" (* :comment-string) 'new-line)

  (rule <javadoc> ::= "/**" 'new-line " *" (* :comment-string) (* <@>) 'new-line " */" 'new-line)
  (rule <javadoc> <@> ::= 'new-line '(" * @" t nil) :name (* :comment-string))

  (rule <import> ::= "import" 'pascal-case :class-or-package
	             (* '("." nil nil) 'pascal-case :name-part) 'semicolon)

  (rule <@> ::= '("@" t nil) 'pascal-case (one-of :name <class-ref>)
	        (? 'open-paren-no-space <arg> (* 'comma <arg>) 'close-paren)
		'new-line)

  (rule <@> <arg> ::= (? :arg-name "=") <expr>)

  (rule <class> ::=
	(unordered (* <import>)
		   'vertical-space
		   (? <javadoc>)
		   <modifiers>
		   "class" 'pascal-case :name
		   (? <template-def>)
		   (? <extends>)
		   (? (group "implements" <implements> (* 'comma <implements>))))
	'begin-block
	(* (one-of <//>
		   <class>
		   <interface>
		   <enum>
		   <field>
		   <static-initializer>
		   <constructor>
		   <method>))
	'end-block)

  (rule <interface> ::=
	(unordered (* <import>)
		   'vertical-space
		   (? <javadoc>)
		   <modifiers>
		   "interface" 'pascal-case :name
		   (? <template-def>)
		   (? (group "extends" <extends> (* 'comma <extends>))))
	'begin-block
	(* (one-of <//>
		   <class>
		   <interface>
		   <enum>
		   <field>
		   <static-initializer>
		   <constructor>
		   <method>))
	'end-block)

  (rule <enum> ::=
	'vertical-space
	(unordered (? <javadoc>)
		   (? <visibility>))
	"enum" 'pascal-case :name 'begin-block
	<constant> (* 'comma 'new-line <constant>)
	(? 'semicolon 'new-line
	   (* (one-of <//>
		      <field>
		      <static-initializer>
		      <constructor>
		      <method>)))
	'end-block)

  (rule <enum> <constant> ::=
	'upper-case :name
	(? <arg-list-1+>))

  (rule <visibility> (naked) ::=
	(one-of <public>
		<protected>
		<private>))

  (rule <modifiers> (naked) ::=
	(unordered (* <@>)
	           (? <visibility>)
		   (? <static>)
		   (? <abstract>)
		   (? <final>)
		   (? <native>)
		   (? <synchronized>)
		   (? <transient>)
		   (? <volatile>)))

  (nullary-rule <public>)
  (nullary-rule <protected>)
  (nullary-rule <private>)
  (nullary-rule <static>)
  (nullary-rule <abstract>)
  (nullary-rule <final>)
  (nullary-rule <native>)
  (nullary-rule <synchronized>)
  (nullary-rule <transient>)
  (nullary-rule <volatile>)

  (rule <extends> ::= "extends" <type-expr>)
  (rule <interface> <extends> ::= <type-expr>)

  (rule <implements> ::= (* <type-expr> (list-separator 'comma)))

  (rule <constructor> ::=
	'vertical-space
	(unordered (? <javadoc>)
		   <modifiers>
		   (group 'pascal-case :name)
		   <param-list>
		   (? (group "throws" <throws> (* 'comma <throws>))))
	'begin-block
	(* <stmt>)
	'end-block)

  (rule <interface> <method> ::=
	'vertical-space
	(unordered (? <javadoc>)
		   <modifiers>
		   (one-of <return-type> "void")
		   (group :name <param-list>)
		   (? (group "throws" <throws> (* 'comma <throws>))))
	'semicolon)

  (rule <method> ::=
	'vertical-space
	(unordered (? <javadoc>)
		   <modifiers>
		   (one-of <return-type> "void")
		   (group :name <param-list>)
		   (? (group "throws" <throws> (* 'comma <throws>))))
	'begin-block
	(* <stmt>)
	'end-block)

  (rule <return-type> ::= <type-expr>)

  (rule <param-list> (naked) ::=
	'open-paren-no-space
	(* <param> (list-separator 'comma))
	'close-paren)

  (rule <param> ::= <type> :name)

  (rule <throws> ::= <type-expr>)

  (rule <field> ::=
	'vertical-space
	(unordered (? <javadoc>)
		   <modifiers>
		   <type>
		   :name
		   (? <init>))
	'semicolon)

  (rule <static-initializer> ::= 'vertical-space "static" 'begin-block (* <stmt>) 'end-block)
  (rule <init> ::= "=" <expr>)

  (rule <type> ::= <type-expr>)
  (rule <type-expr> (naked) ::= (one-of (group 'pascal-case :type-name (? <template>))
					(group <class-ref> (? <template>))
					<array>))
  (rule <array> ::= <type-expr> '("[]" nil nil))

  (rule <template> ::= '("<" nil nil) (* (one-of (group 'pascal-case :arg) <class-ref>) (list-separator 'comma)) '(">" nil t))
  (rule <template-def> ::= '("<" nil nil) (* <param> (list-separator 'comma)) '(">" nil t))
  (rule <template-def> <param> ::= 'upper-case :param (? <extends>))
  (rule <template-def> <param> <extends> ::= "extends" <type-expr>)

  (rule <arg-list> (naked) ::=
	'open-paren-no-space
	(* <expr> (list-separator 'comma))
	'close-paren)

  (rule <arg-list-1+> (naked) ::=
	'open-paren-no-space
	<expr> (* 'comma <expr>)
	'close-paren)

  (rule <expr> (naked) ::=
	(one-of :value <char-literal> <string-literal> <array-literal> <class-ref>
		<group>
		<array-ref> <new> <call> <call-this> <call-super> <field-ref> <field-ref-this> <field-ref-super> <class-object>
		<post++> <post-->
		<++> <--> <positive> <negative> <~> <!> <cast>
		<*> </> <%>
		<+> <->
		<<<> <>>> <>>>>
		<<> <>> <<=> <>=> <instanceof>
		<==> <!=>
		<bit-and>
		<bit-xor>
		<bit-or>
		<and>
		<or>
		<?>
		<set> <=> <+=> <-=> <*=> </=> <%=> <bit-and=> <bit-xor=> <bit-or=> <<<=> <>>=> <>>>=>
		<progn>))

  (rule <char-literal> ::= '("'" t nil) 'suppress-wrap :value '("'" nil t nil))
  (rule <string-literal> ::= '("\"" t nil) 'suppress-wrap :value '("\"" nil t nil))
  (rule <array-literal> ::= "{" (* <expr> (list-separator 'comma)) "}")
  (rule <class-ref> ::= 'pascal-case :identifier (* '("." nil nil) 'pascal-case :identifier))
  (rule <class-or-field> (naked) ::= (one-of :identifier <class-ref>))

  (precedence-rules <expr>

    ;; grouping
    ((rule <group> ::= <expr>))

    ;; postfix and accessors
    ((rule <post++> ::= <expr> '("++" nil t))
     (rule <post--> ::= <expr> '("--" nil t))
     (rule <array-ref> ::= <expr> (* '("[" nil nil) <expr> '("]" nil t)))
     (rule <field-ref> ::= <expr> '("." nil nil) <class-or-field> (* '("." nil nil) <class-or-field>))
     (rule <field-ref-this> ::= '("this." t nil) <class-or-field>)
     (rule <field-ref-super> ::= '("super." t nil) <class-or-field>)
     (rule <class-object> ::= 'pascal-case :identifier (* '("." nil nil) 'pascal-case :identifier) '(".class" nil t)))

    ;; unary
    ((rule <++> ::= '("++" t nil) <expr>)
     (rule <--> ::= '("--" t nil) <expr>)
     (rule <positive> ::= '("+" t nil) <expr>)
     (rule <negative> ::= '("-" t nil) <expr>)
     (rule <~> ::= '("~" t nil) <expr>)
     (rule <!> ::= '("!" t nil) <expr>)
     (rule <new> ::= "new" <type-expr> <arg-list>)
     (rule <cast> (assoc :right) ::= 'open-paren <type-expr> 'close-paren <expr>))

    ;; multiplicative
    ((rule <*> ::= <expr> "*" <expr> (* "*" <expr>))
     (rule </> ::= <expr> "/" <expr>)
     (rule <%> ::= <expr> "%" <expr>))

    ;; additive
    ((rule <+> ::= <expr> "+" <expr> (* "+" <expr>))
     (rule <-> ::= <expr> "-" <expr> (* "-" <expr>)))

    ;; shift
    ((rule <<<> ::= <expr> "<<" <expr>)
     (rule <>>> ::= <expr> ">>" <expr>)
     (rule <>>>> ::= <expr> ">>>" <expr>))

    ;; relational
    ((rule <<> ::= <expr> "<" <expr>)
     (rule <>> ::= <expr> ">" <expr>)
     (rule <<=> ::= <expr> "<=" <expr>)
     (rule <>=> ::= <expr> ">=" <expr>)
     (rule <instanceof> ::= <expr> "instanceof" 'pascal-case <expr>))

    ;; equality
    ((rule <==> ::= <expr> "==" <expr>)
     (rule <!=> ::= <expr> "!=" <expr>))

    ;; bitwise AND
    ((rule <bit-and> ::= <expr> "&" <expr>))

    ;; bitwise XOR
    ((rule <bit-xor> ::= <expr> "^" <expr>))

    ;; bitwise OR
    ((rule <bit-or> ::= <expr> "|" <expr>))

    ;; logical AND
    ((rule <and> ::= <expr> "&&" <expr>))

    ;; logical OR
    ((rule <or> ::= <expr> "||" <expr>))

    ;; conditional
    ((rule <?> (assoc :right) ::= <expr> "?" 'push-wrap-marker <expr> ":" <expr> 'pop-wrap-marker))

    ;; assignment
    ((rule <set> (assoc :right) ::= <expr> "=" <expr>)
     (rule <=> (assoc :right) ::= <expr> "=" <expr>)
     (rule <+=> (assoc :right) ::= <expr> "+=" <expr>)
     (rule <-=> (assoc :right) ::= <expr> "-=" <expr>)
     (rule <*=> (assoc :right) ::= <expr> "*=" <expr>)
     (rule </=> (assoc :right) ::= <expr> "/=" <expr>)
     (rule <%=> (assoc :right) ::= <expr> "%=" <expr>)
     (rule <bit-and=> (assoc :right) ::= <expr> "&=" <expr>)
     (rule <bit-xor=> (assoc :right) ::= <expr> "^=" <expr>)
     (rule <bit-or=> (assoc :right) ::= <expr> "|=" <expr>)
     (rule <<<=> (assoc :right) ::= <expr> "<<=" <expr>)
     (rule <>>=> (assoc :right) ::= <expr> ">>=" <expr>)
     (rule <>>>=> (assoc :right) ::= <expr> ">>>=" <expr>))

    ;; comma
    ((rule <progn> ::= <expr> (* 'comma <expr>))))

  (rule <call> ::= <expr> '("." nil nil) :identifier <arg-list>)
  (rule <call-this> ::= '("this." t nil) :identifier <arg-list>)
  (rule <call-super> ::= '("super." t nil) :identifier <arg-list>)

  (rule <stmt> (naked) ::= (one-of <_> <//> <stmt-no-var-while> <var> <while>))
  (rule <stmt-no-while> (naked) ::= (one-of <_> <//> <stmt-no-var-while> <var>))
  (rule <stmt-no-var> (naked) ::= (one-of <_> <//> <stmt-no-var-while> <while>))
  (rule <stmt-no-var-while> (naked) ::=
	(one-of (group (one-of <expr> <return> <goto> <throw> <break> <continue> <assert>) 'semicolon)
		<block> <synchronized-block> <if> <for> <for-each> <do> <switch> <try> <label>
		<this-constructor> <super-constructor>))

  (rule <var> ::=
	(unordered <modifiers> <type>)
	:name
	(? <init>)
	'semicolon)

  (rule <for.for-each.while.progn> <var> ::= <type> :name (? <init>))

  (rule <_> ::= 'vertical-space)

  (rule <return> ::= "return" <expr>)
  (rule <goto> ::= "goto" 'pascal-case :label)
  (rule <throw> ::= "throw" <expr>)
  (rule <break> ::= "break" (? 'pascal-case :label))
  (rule <continue> ::= "continue" (? 'pascal-case :label))
  (rule <assert> ::= "assert" <expr> (? ":" <expr>))

  (rule <block> ::=
	(one-of (group <stmt-no-var> (* <stmt-no-var>))
		(group 'begin-block (* <stmt>) 'end-block)))

  (rule <if.for.for-each.while.do.try> <block> ::= (* <stmt>))

  (rule <synchronized-block> ::= "synchronized" 'open-paren <expr> 'close-paren 'begin-block (* <stmt>) 'end-block)

  (rule <if> ::=
	"if" 'open-paren <expr> 'close-paren 'begin-block
	<stmt> 'end-block-no-new-line
	(? "else"
	   (one-of <if>
		   (group 'begin-block <stmt> 'end-block)))
	'new-line)

  (rule <for> ::=
	"for" 'open-paren
	(one-of <progn> <var> <expr> <_>) '(";" nil t)
	(one-of <expr> <_>) '(";" nil t)
	(one-of <expr> <_>) 'close-paren 'begin-block
	(* <stmt>) 'end-block)

  (rule <for> <progn> ::= (* (one-of <var> <expr>) (list-separator 'comma)))
  (rule <for> <_> ::= '("" nil nil))

  (rule <for-each> ::=
	"for" 'open-paren
	(one-of <progn> <var> <expr>) '(":" nil t)
	<expr> 'close-paren 'begin-block
	(* <stmt>) 'end-block)

  (rule <while> ::= "while" 'open-paren (one-of <progn> <var> <expr>) 'close-paren (* <stmt>))

  (rule <do> ::= "do" 'begin-block (* <stmt-no-while>) 'end-block-no-new-line <while> 'semicolon)
  (rule <do> <while> ::= "while" 'open-paren <expr> 'close-paren)

  (rule <switch> ::=
	"switch" 'open-paren <expr> 'close-paren 'begin-block 'decrease-indent
	(one-of <case> <default>)
	(* (one-of <case> <default>))
	'increase-indent 'end-block)

  (rule <switch-case-body> (naked) ::=
	'(":" nil t)
	'increase-indent 'new-line
	(* <stmt>)
	"break" 'semicolon
	'decrease-indent)

  (rule <switch> <case> ::= 'vertical-space "case" :literal <switch-case-body>)
  (rule <switch> <default> ::= 'vertical-space "default" <switch-case-body>)

  (rule <try> ::=
	"try" 'begin-block (* <stmt>)
	(* <catch>)
	(? <finally>)
	'end-block)

  (rule <try> <catch>  ::= 'end-block-no-new-line "catch" 'open-paren <type-expr> :name 'close-paren 'begin-block (* <stmt>))
  (rule <try> <finally> ::= 'end-block-no-new-line "finally" 'begin-block (* <stmt>))

  (rule <label> ::= 'vertical-space 'pascal-case :label '(":" nil t) 'new-line)

  (rule <this-constructor> ::= "this" <arg-list> 'semicolon)
  (rule <super-constructor> ::= "super" <arg-list> 'semicolon)

  ;; Make + and - polymorphic: both unary and n-ary.
  (macro <+> ::= (if (third +) `(,+) `((positive ,(second +)))))
  (macro <-> ::= (if (third -) `(,-) `((negative ,(second -)))))

  ;; Fill in the name in constructors.
  (macro <class> ::=
	 (list (mapcar #'(lambda (class-entry)
			   (if (is-zexp-of-type class-entry 'constructor)
			       `(constructor ,(get-zexp-name class) ,@(rest class-entry))
			       class-entry))
		       class)))

  (macro <enum> ::=
	 (list (mapcar #'(lambda (enum-entry)
			   (if (is-zexp-of-type enum-entry 'constructor)
			       `(constructor ,(get-zexp-name enum) ,@(rest enum-entry))
			       enum-entry))
		       enum)))

  ;; cond: if/else if.../else.
  (macro <cond> ::=
	 (labels ((rec (remaining-clauses)
		    (let ((clause (first remaining-clauses)))
		      (if (eq (first clause) 't)
			  (second clause)
			  `(if ,(first clause)
			    ,(second clause)
			    ,@(awhen (rest remaining-clauses)
				     (list (rec it))))))))
	   (list (rec (rest cond)))))

  ;; Properties.
  (macro <prop> ::= ; TODO: macro schema (macro <name> ::= schema :=> definition.
	 (let ((name (get-zexp-name prop))
	       (type (spath prop 'type))
	       (init (spath prop 'init))
	       (doc (spath prop 'doc))
	       (protected-p (spath prop 'protected)))
	   `((field ,name ,(if protected-p '(protected) '(private)) ,type
	      ,@(awhen init (list it))
	      ,@(when doc (list `(javadoc ,@(rest doc)))))
	     (method ,(gen-symbol :keyword :get (second prop))
	      (public) (return-type ,@(rest type))
	      (return (field-ref-this ,name)))
	     (method ,(gen-symbol :keyword :set (second prop))
	      (public) (param :value ,type)
	      (set (field-ref-this ,name) :value)))))

  ;; JavaDoc macros.
  (macro <field> ::=
	 (list (mapcar #'(lambda (field-entry)
			   (if (is-zexp-of-type field-entry 'doc)
			       `(javadoc ,@(rest field-entry))
			       field-entry))
		       field)))

  (macro <method> ::=
	 (let* ((method-doc (spath method 'doc))
	        (params (spath method 'param*))
	        (has-param-docs (find-if #'(lambda (param) (spath param 'doc)) params))
	        (return-doc (spath method '(return-type doc)))
		(throws (spath method 'throws*))
	        (has-throws-docs (find-if #'(lambda (throws) (spath throws 'doc)) throws))
		(process-name #'(lambda (x) (if (stringp x) x (symbol->camel-string x))))
		(process-javadoc-tag #'(lambda (type instance)
					 `(@ ,type ,(apply #'string+
							   (funcall process-name (second instance))
							   " "
							   (awhen (spath instance 'doc)
							     (rest it)))))))
	   `((method ,(get-zexp-name method)
	      ,@(when (or method-doc has-param-docs return-doc has-throws-docs)
		      `((javadoc
			 ,@(rest method-doc)
			 ,@(mapcar (curry process-javadoc-tag :param) params)
			 ,@(awhen return-doc
				  `((@ :return ,@(rest return-doc))))
			 ,@(mapcar (curry process-javadoc-tag :throws) throws))))
	      ,@(mappend #'(lambda (method-entry)
			     (case (get-zexp-tag method-entry)
			       (doc nil)
			       ((param return-type throws)
				(list (zexp-remove-properties method-entry 'doc)))
			       (otherwise (list method-entry))))
			 (drop 2 method))))))

  (macro <class> ::=
	 (list (mapcar #'(lambda (class-entry)
			   (if (is-zexp-of-type class-entry 'doc)
			       `(javadoc ,@(rest class-entry))
			       class-entry))
		       class)))

  (macro <interface> ::=
	 (list (mapcar #'(lambda (interface-entry)
			   (if (is-zexp-of-type interface-entry 'doc)
			       `(javadoc ,@(rest interface-entry))
			       interface-entry))
		       interface)))

  (macro <enum> ::=
	 (list (mapcar #'(lambda (enum-entry)
			   (if (is-zexp-of-type enum-entry 'doc)
			       `(javadoc ,@(rest enum-entry))
			       enum-entry))
		       enum))))


(defun make-java-source-generator (&optional (file-name *standard-output*))
  (make-instance 'source-file-generator
		 :output-file file-name
		 :keyword-overrides '((:void "void")
				      (:byte "byte")
				      (:short "short")
				      (:int "int")
				      (:long "long")
				      (:float "float")
				      (:double "double")
				      (:char "char")
				      (:boolean "boolean")
				      (:true "true")
				      (:false "false"))))


;;; Entry point.
(defun generate-java (package &key output-dir create-dir-structure print-filenames)
  (flet ((gen-package-path-string (separator)
           (apply (compose #'string-downcase #'string+)
                  (add-separators separator
                                  (mapcar #'(lambda (s)
                                              (remove-if-not #'alphanumericp (symbol-name s)))
                                          (cadr (spath package :value)))))))
    (let ((package-name (gen-package-path-string "."))
          (package-dir (parse-namestring (string+ (if create-dir-structure
                                                      (gen-package-path-string "/")
                                                      ".")
                                                  "/")))
          (output-dir (or (when output-dir
                            (if (eql (last output-dir) #\\)
                                output-dir
                                (string+ output-dir "/")))
                          #p"./")))
      (mapc #'(lambda (interface-or-class)
                (let ((filename
                       (merge-pathnames
                        (merge-pathnames package-dir output-dir) ; Final directory.
                        (parse-namestring                        ; Final filename: ClassName.java
                         (string+
                          (reduce #'string+
                                  (mapcar #'string-capitalize
                                          (split-sequence:split-sequence
                                           #\-
                                           (symbol-name (spath interface-or-class :name)))))
                          ".java")))))
                  (if print-filenames
                      (format t "~&~A~&" (file-namestring filename))
                      (progn
                        (ensure-directories-exist filename :verbose t)
                        (format t "Generating ~A...~%" (file-namestring filename))
                        (with-open-file (class-file filename :direction :output
                                                    :if-exists :supersede)
                          (let ((gen (make-java-source-generator class-file)))
                            (emit-list gen "package" package-name 'semicolon 'vertical-space)
                            (generate-sexp gen #'java-parse interface-or-class)))))))
            (append (spath package 'interface*)
                    (spath package 'class*))))))

(export 'generate-java)


;;; Simple grammar test.
(def-grammar test
  (rule <class> ::= "class" (unordered <a> (? <b>) (group "name" :name)) (* <field>))
  (rule <a> ::= "a")
  (rule <b> ::= "b")
  (rule <field> ::= "field" :name)
  (rule <-> ::= (one-of <-> <a>) "-" (one-of (group 'open-paren <-> 'close-paren) <a>))
  (macro <prop> ::=
	 `((field :f1) (field :f2))))

(test "simple-grammar-test"
      '("class" "a" "b" "name" :C "field" :F1 "field" :F2)
      (catharsis::parse-result-gen-list (funcall 'test-parse '(class :c (b) (a) (prop)))))


;;;; Test.
(define-constant +java-test+
  '((class :my-class
     (extends "IOException" (template :t))
     (abstract) (public)
     (doc "Class documentation..." (@ :see "SomethingElse"))

     (import "java.io.IOException")
     (import "java.io.Writer")
     (import "google.com.fubar.data" :data-class)

     (template-def
      (param :t)
      (param :e (extends :entry)))

     (field :my-field (type :int) (private) (synchronized) (init 2)
      (doc "Field documentation..."))
     (prop :my-prop (type :int) (init 0))

     (static-initializer
      (set :my-field 3))

     (enum :simple-enum
      (constant :first)
      (constant :second))

     (enum :complex-enum
      (constant :mercury 3.303 2.439)
      (constant :venus 4.869 6.052)
      (field :mass (private) (final) (type :double))
      (field :radius (private) (final) (type :double))
      (constructor
       (param :mass (type :double))
       (param :radius (type :double))
       (set (field-ref-this :mass) :mass)
       (set (field-ref-this :radius) :radius)))

     (constructor (public) (this-constructor))
     (constructor (throws :exception) (param :param-1 (type :string)) (protected)
      (super-constructor :arg-1 :arg-2))

     (method :my-method (return-type :string (doc "Return value.")) (public)
      (doc "This method does strange things.")
      (param :param-1 (type :int) (doc "First param."))
      (param :param-2 (type :string))
      (throws "java.io.IOException" (doc "Some exception."))
      (throws :runtime-exception)

      (var :my-var (type :int) (init 0))
      (var :my-array
	   (type (array :int))
	   (init (new (array :int) (array-literal 1 2 3))))
      (_)
      (if (== :my-var 0)
	  (call-this :my-method-2)
	  (block			;(doc "Complex block.")
	    (set :my-var (? (<= :my-var 100) 1 2))
	    (set :my-field-2 :null)
	    (call :my-var :count-chars :this (field-ref :my-var-2 :my-field-2) (+ 1 (field-ref-this :my-field)))
	    (call-this :void-fn)
	    (set :my-field-2 (new :my-class (string-literal "abc")))))
      (_)
      (for (var :i (type :int) (init 0)) (< :i "MAX") (++ :i)
	   (call "System.out" :println (array-ref (field-ref-this :my-array) :i))
	   (call-this :flush))
      (_)
      (synchronized-block :this
			  (for-each (var :x (type :int))
				    (field-ref-this :my-list)
				    (call-this :flush)))
      (_)
      (switch (call-this :get-name)
	      (case "FIRST"
		(++ :x))
	      (default
		  (-- :x)))
      (_)
      (// "Block with var.")
      (block
	  (var :i (type :int))
	(call-this :flush)
	(call-this :reset))
      (_)
      (// "Block without var.")
      (block
	  (call-this :initialize))
      (_)
      (do
       (set :my-var (+ :my-var 1))
       (while (< (cast :int :my-var) 10)))
      (_)
      (try
       (call-this :flush)
       (call-this :reset)
       (catch :e :exception-type-1
	      (call-this :handle-exception-1))
       (catch :e :exception-type-2
	      (call-this :handle-exception-2))
       (catch :e :exception-type-3)
       (finally
	(call-this :handle-finally)))
      (_)
      (cond ((== (+ :my-var 1) 0) (call-super :my-method-2) (set :my-var 1))
	    ((> :my-var 0) (set :my-var 0))
	    (t (set :my-var 1))))
     (field :my-field
      (public)
      (type :my-type)
      (init (? (+ :v1 :v2 :v3)
	       (negative (* (group (+ :x :y)) (+ :z :w)))
	       (post++ (++ :x)))))
     (field :my-field-2
      (public)
      (type :my-type)
      (init (progn (string-literal "abc") (array-ref :arr (+ (progn :i1 :i2) :i3) :i4))))
     (field :my-field-3
      (public)
      (type :my-type)
      (init (new :int 1 (cast :string (array-ref :arr :i1 :i2)) 2 3)))
     (enum :my-enum
      (constant :c1)
      (constant :c2)
      (constant :c3))
     (field :my-array
      (type :my-type)
      (init (array-literal (- :v1 (- :v3 :v4)) (- (- :v1 :v2) :v3) (+ (- 42) :v5))))
     (field :my-field-2 (type :string) (public))
     (method :my-method-2)
     (prop :my-prop (type :string) (doc "Property documentation...")))
    (interface :my-interface (public)
	     (extends :another-interface) (extends :yes-another-interface)
	     (import "java.io.Writer")
	     (method :my-method
		     (return-type :template-class (template :f :e))
		     (param :my-param (type :int))
		     (throws "RuntimeException")))))

;;; Test for hand execution.
(eval-when (:execute)
  (prog1 nil
    (dolist (java-form +java-test+)
      (generate-sexp (make-java-source-generator) #'java-parse java-form)
      (format t "~3%"))))

;;; Real test.
(test "parse-java"
      '("import" PASCAL-CASE "java.io.IOException" SEMICOLON "import"
	PASCAL-CASE "java.io.Writer" SEMICOLON "import" PASCAL-CASE
	"google.com.fubar.data" ("." NIL NIL) PASCAL-CASE :DATA-CLASS
	SEMICOLON VERTICAL-SPACE "/**" NEW-LINE " *"
	"Class documentation..." NEW-LINE (" * @" T NIL) :SEE
	"SomethingElse" NEW-LINE " */" NEW-LINE "public" "abstract" "class"
	PASCAL-CASE :MY-CLASS ("<" NIL NIL) UPPER-CASE :T COMMA UPPER-CASE
	:E "extends" PASCAL-CASE :ENTRY (">" NIL T) "extends" PASCAL-CASE
	"IOException" ("<" NIL NIL) PASCAL-CASE :T (">" NIL T) BEGIN-BLOCK
	VERTICAL-SPACE "/**" NEW-LINE " *" "Field documentation..." NEW-LINE
	" */" NEW-LINE "private" "synchronized" PASCAL-CASE :INT :MY-FIELD
	"=" 2 SEMICOLON VERTICAL-SPACE "private" PASCAL-CASE :INT :MY-PROP
	"=" 0 SEMICOLON VERTICAL-SPACE "public" PASCAL-CASE :INT
	:GET-MY-PROP OPEN-PAREN-NO-SPACE CLOSE-PAREN BEGIN-BLOCK "return"
	("this." T NIL) :MY-PROP SEMICOLON END-BLOCK VERTICAL-SPACE "public"
	"void" :SET-MY-PROP OPEN-PAREN-NO-SPACE PASCAL-CASE :INT :VALUE
	CLOSE-PAREN BEGIN-BLOCK ("this." T NIL) :MY-PROP "=" :VALUE
	SEMICOLON END-BLOCK VERTICAL-SPACE "static" BEGIN-BLOCK :MY-FIELD
	"=" 3 SEMICOLON END-BLOCK VERTICAL-SPACE "enum" PASCAL-CASE
	:SIMPLE-ENUM BEGIN-BLOCK UPPER-CASE :FIRST COMMA NEW-LINE UPPER-CASE
	:SECOND SEMICOLON NEW-LINE END-BLOCK VERTICAL-SPACE "enum"
	PASCAL-CASE :COMPLEX-ENUM BEGIN-BLOCK UPPER-CASE :MERCURY
	OPEN-PAREN-NO-SPACE 3.303 COMMA 2.439 CLOSE-PAREN COMMA NEW-LINE
	UPPER-CASE :VENUS OPEN-PAREN-NO-SPACE 4.869 COMMA 6.052 CLOSE-PAREN
	SEMICOLON NEW-LINE VERTICAL-SPACE "private" "final" PASCAL-CASE
	:DOUBLE :MASS SEMICOLON VERTICAL-SPACE "private" "final" PASCAL-CASE
	:DOUBLE :RADIUS SEMICOLON VERTICAL-SPACE PASCAL-CASE :COMPLEX-ENUM
	OPEN-PAREN-NO-SPACE PASCAL-CASE :DOUBLE :MASS COMMA PASCAL-CASE
	:DOUBLE :RADIUS CLOSE-PAREN BEGIN-BLOCK ("this." T NIL) :MASS "="
	:MASS SEMICOLON ("this." T NIL) :RADIUS "=" :RADIUS SEMICOLON
	END-BLOCK END-BLOCK VERTICAL-SPACE "public" PASCAL-CASE :MY-CLASS
	OPEN-PAREN-NO-SPACE CLOSE-PAREN BEGIN-BLOCK "this"
	OPEN-PAREN-NO-SPACE CLOSE-PAREN SEMICOLON END-BLOCK VERTICAL-SPACE
	"protected" PASCAL-CASE :MY-CLASS OPEN-PAREN-NO-SPACE PASCAL-CASE
	:STRING :PARAM-1 CLOSE-PAREN "throws" PASCAL-CASE :EXCEPTION
	BEGIN-BLOCK "super" OPEN-PAREN-NO-SPACE :ARG-1 COMMA :ARG-2
	CLOSE-PAREN SEMICOLON END-BLOCK VERTICAL-SPACE "/**" NEW-LINE " *"
	"This method does strange things." NEW-LINE (" * @" T NIL) :PARAM
	"param1 First param." NEW-LINE (" * @" T NIL) :PARAM "param2 "
	NEW-LINE (" * @" T NIL) :RETURN "Return value." NEW-LINE
	(" * @" T NIL) :THROWS "java.io.IOException Some exception."
	NEW-LINE (" * @" T NIL) :THROWS "runtimeException " NEW-LINE " */"
	NEW-LINE "public" PASCAL-CASE :STRING :MY-METHOD OPEN-PAREN-NO-SPACE
	PASCAL-CASE :INT :PARAM-1 COMMA PASCAL-CASE :STRING :PARAM-2
	CLOSE-PAREN "throws" PASCAL-CASE "java.io.IOException" COMMA
	PASCAL-CASE :RUNTIME-EXCEPTION BEGIN-BLOCK PASCAL-CASE :INT :MY-VAR
	"=" 0 SEMICOLON PASCAL-CASE :INT ("[]" NIL NIL) :MY-ARRAY "=" "new"
	PASCAL-CASE :INT ("[]" NIL NIL) OPEN-PAREN-NO-SPACE "{" 1 COMMA 2
	COMMA 3 "}" CLOSE-PAREN SEMICOLON VERTICAL-SPACE "if" OPEN-PAREN
	:MY-VAR "==" 0 CLOSE-PAREN BEGIN-BLOCK ("this." T NIL) :MY-METHOD-2
	OPEN-PAREN-NO-SPACE CLOSE-PAREN SEMICOLON END-BLOCK-NO-NEW-LINE
	"else" BEGIN-BLOCK :MY-VAR "=" :MY-VAR "<=" 100 "?" PUSH-WRAP-MARKER
	1 ":" 2 POP-WRAP-MARKER SEMICOLON :MY-FIELD-2 "=" :NULL SEMICOLON
	:MY-VAR ("." NIL NIL) :COUNT-CHARS OPEN-PAREN-NO-SPACE :THIS COMMA
	:MY-VAR-2 ("." NIL NIL) :MY-FIELD-2 COMMA 1 "+" ("this." T NIL)
	:MY-FIELD CLOSE-PAREN SEMICOLON ("this." T NIL) :VOID-FN
	OPEN-PAREN-NO-SPACE CLOSE-PAREN SEMICOLON :MY-FIELD-2 "=" "new"
	PASCAL-CASE :MY-CLASS OPEN-PAREN-NO-SPACE ("\"" T NIL) SUPPRESS-WRAP
	"abc" ("\"" NIL T NIL) CLOSE-PAREN SEMICOLON END-BLOCK NEW-LINE
	VERTICAL-SPACE "for" OPEN-PAREN PASCAL-CASE :INT :I "=" 0
	(";" NIL T) :I "<" "MAX" (";" NIL T) ("++" T NIL) :I CLOSE-PAREN
	BEGIN-BLOCK "System.out" ("." NIL NIL) :PRINTLN OPEN-PAREN-NO-SPACE
	("this." T NIL) :MY-ARRAY ("[" NIL NIL) :I ("]" NIL T) CLOSE-PAREN
	SEMICOLON ("this." T NIL) :FLUSH OPEN-PAREN-NO-SPACE CLOSE-PAREN
	SEMICOLON END-BLOCK VERTICAL-SPACE "synchronized" OPEN-PAREN :THIS
	CLOSE-PAREN BEGIN-BLOCK "for" OPEN-PAREN PASCAL-CASE :INT :X
	(":" NIL T) ("this." T NIL) :MY-LIST CLOSE-PAREN BEGIN-BLOCK
	("this." T NIL) :FLUSH OPEN-PAREN-NO-SPACE CLOSE-PAREN SEMICOLON
	END-BLOCK END-BLOCK VERTICAL-SPACE "switch" OPEN-PAREN
	("this." T NIL) :GET-NAME OPEN-PAREN-NO-SPACE CLOSE-PAREN
	CLOSE-PAREN BEGIN-BLOCK DECREASE-INDENT VERTICAL-SPACE "case"
	"FIRST" (":" NIL T) INCREASE-INDENT NEW-LINE ("++" T NIL) :X
	SEMICOLON "break" SEMICOLON DECREASE-INDENT VERTICAL-SPACE "default"
	(":" NIL T) INCREASE-INDENT NEW-LINE ("--" T NIL) :X SEMICOLON
	"break" SEMICOLON DECREASE-INDENT INCREASE-INDENT END-BLOCK
	VERTICAL-SPACE "//" "Block with var." NEW-LINE BEGIN-BLOCK
	PASCAL-CASE :INT :I SEMICOLON ("this." T NIL) :FLUSH
	OPEN-PAREN-NO-SPACE CLOSE-PAREN SEMICOLON ("this." T NIL) :RESET
	OPEN-PAREN-NO-SPACE CLOSE-PAREN SEMICOLON END-BLOCK VERTICAL-SPACE
	"//" "Block without var." NEW-LINE ("this." T NIL) :INITIALIZE
	OPEN-PAREN-NO-SPACE CLOSE-PAREN SEMICOLON VERTICAL-SPACE "do"
	BEGIN-BLOCK :MY-VAR "=" :MY-VAR "+" 1 SEMICOLON
	END-BLOCK-NO-NEW-LINE "while" OPEN-PAREN OPEN-PAREN PASCAL-CASE :INT
	CLOSE-PAREN :MY-VAR "<" 10 CLOSE-PAREN SEMICOLON VERTICAL-SPACE
	"try" BEGIN-BLOCK ("this." T NIL) :FLUSH OPEN-PAREN-NO-SPACE
	CLOSE-PAREN SEMICOLON ("this." T NIL) :RESET OPEN-PAREN-NO-SPACE
	CLOSE-PAREN SEMICOLON END-BLOCK-NO-NEW-LINE "catch" OPEN-PAREN
	PASCAL-CASE :EXCEPTION-TYPE-1 :E CLOSE-PAREN BEGIN-BLOCK
	("this." T NIL) :HANDLE-EXCEPTION-1 OPEN-PAREN-NO-SPACE CLOSE-PAREN
	SEMICOLON END-BLOCK-NO-NEW-LINE "catch" OPEN-PAREN PASCAL-CASE
	:EXCEPTION-TYPE-2 :E CLOSE-PAREN BEGIN-BLOCK ("this." T NIL)
	:HANDLE-EXCEPTION-2 OPEN-PAREN-NO-SPACE CLOSE-PAREN SEMICOLON
	END-BLOCK-NO-NEW-LINE "catch" OPEN-PAREN PASCAL-CASE
	:EXCEPTION-TYPE-3 :E CLOSE-PAREN BEGIN-BLOCK END-BLOCK-NO-NEW-LINE
	"finally" BEGIN-BLOCK ("this." T NIL) :HANDLE-FINALLY
	OPEN-PAREN-NO-SPACE CLOSE-PAREN SEMICOLON END-BLOCK VERTICAL-SPACE
	"if" OPEN-PAREN :MY-VAR "+" 1 "==" 0 CLOSE-PAREN BEGIN-BLOCK
	("super." T NIL) :MY-METHOD-2 OPEN-PAREN-NO-SPACE CLOSE-PAREN
	SEMICOLON END-BLOCK-NO-NEW-LINE "else" "if" OPEN-PAREN :MY-VAR ">" 0
	CLOSE-PAREN BEGIN-BLOCK :MY-VAR "=" 0 SEMICOLON
	END-BLOCK-NO-NEW-LINE "else" BEGIN-BLOCK :MY-VAR "=" 1 SEMICOLON
	END-BLOCK NEW-LINE NEW-LINE END-BLOCK VERTICAL-SPACE "public"
	PASCAL-CASE :MY-TYPE :MY-FIELD "=" :V1 "+" :V2 "+" :V3 "?"
	PUSH-WRAP-MARKER ("-" T NIL) OPEN-PAREN OPEN-PAREN :X "+" :Y
	CLOSE-PAREN "*" OPEN-PAREN :Z "+" :W CLOSE-PAREN CLOSE-PAREN ":"
	OPEN-PAREN ("++" T NIL) :X CLOSE-PAREN ("++" NIL T) POP-WRAP-MARKER
	SEMICOLON VERTICAL-SPACE "public" PASCAL-CASE :MY-TYPE :MY-FIELD-2
	"=" ("\"" T NIL) SUPPRESS-WRAP "abc" ("\"" NIL T NIL) COMMA :ARR
	("[" NIL NIL) OPEN-PAREN OPEN-PAREN :I1 COMMA :I2 CLOSE-PAREN "+"
	:I3 CLOSE-PAREN ("]" NIL T) ("[" NIL NIL) :I4 ("]" NIL T) SEMICOLON
	VERTICAL-SPACE "public" PASCAL-CASE :MY-TYPE :MY-FIELD-3 "=" "new"
	PASCAL-CASE :INT OPEN-PAREN-NO-SPACE 1 COMMA OPEN-PAREN PASCAL-CASE
	:STRING CLOSE-PAREN :ARR ("[" NIL NIL) :I1 ("]" NIL T) ("[" NIL NIL)
	:I2 ("]" NIL T) COMMA 2 COMMA 3 CLOSE-PAREN SEMICOLON VERTICAL-SPACE
	"enum" PASCAL-CASE :MY-ENUM BEGIN-BLOCK UPPER-CASE :C1 COMMA
	NEW-LINE UPPER-CASE :C2 COMMA NEW-LINE UPPER-CASE :C3 SEMICOLON
	NEW-LINE END-BLOCK VERTICAL-SPACE PASCAL-CASE :MY-TYPE :MY-ARRAY "="
	"{" :V1 "-" OPEN-PAREN :V3 "-" :V4 CLOSE-PAREN COMMA :V1 "-" :V2 "-"
	:V3 COMMA ("-" T NIL) 42 "+" :V5 "}" SEMICOLON VERTICAL-SPACE
	"public" PASCAL-CASE :STRING :MY-FIELD-2 SEMICOLON VERTICAL-SPACE
	"void" :MY-METHOD-2 OPEN-PAREN-NO-SPACE CLOSE-PAREN BEGIN-BLOCK
	END-BLOCK VERTICAL-SPACE "/**" NEW-LINE " *"
	"Property documentation..." NEW-LINE " */" NEW-LINE "private"
	PASCAL-CASE :STRING :MY-PROP SEMICOLON VERTICAL-SPACE "public"
	PASCAL-CASE :STRING :GET-MY-PROP OPEN-PAREN-NO-SPACE CLOSE-PAREN
	BEGIN-BLOCK "return" ("this." T NIL) :MY-PROP SEMICOLON END-BLOCK
	VERTICAL-SPACE "public" "void" :SET-MY-PROP OPEN-PAREN-NO-SPACE
	PASCAL-CASE :STRING :VALUE CLOSE-PAREN BEGIN-BLOCK ("this." T NIL)
	:MY-PROP "=" :VALUE SEMICOLON END-BLOCK END-BLOCK "import"
	PASCAL-CASE "java.io.Writer" SEMICOLON VERTICAL-SPACE "public"
	"interface" PASCAL-CASE :MY-INTERFACE "extends" PASCAL-CASE
	:ANOTHER-INTERFACE COMMA PASCAL-CASE :YES-ANOTHER-INTERFACE
	BEGIN-BLOCK VERTICAL-SPACE PASCAL-CASE :TEMPLATE-CLASS ("<" NIL NIL)
	PASCAL-CASE :F COMMA PASCAL-CASE :E (">" NIL T) :MY-METHOD
	OPEN-PAREN-NO-SPACE PASCAL-CASE :INT :MY-PARAM CLOSE-PAREN "throws"
	PASCAL-CASE "RuntimeException" SEMICOLON END-BLOCK)
      (mapcan (compose #'catharsis::parse-result-gen-list
		       #'java-parse)
	      +java-test+))
