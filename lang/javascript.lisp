(in-package :com.google.catharsis.javascript)


(def-grammar javascript

  (rule <module> ::= "// GENERATED CODE: " 'pascal-case :name 'new-line (* <stmt>))

  (rule <//> ::= "//" (* :comment-string) 'new-line)

  (rule <params> ::= (* :identifier (list-separator 'comma)))

  (rule <arg-list> (naked) ::=
	'open-paren-no-space
	(* <expr> (list-separator 'comma))
	'close-paren)

  (rule <arg-list-with-nil> (naked) ::=
	'open-paren-no-space
	"null"
	(* 'comma <expr>)
	'close-paren)

  (rule <type-expr> (naked) ::=
	(one-of <type-ref>
		<field-ref>
		(group 'pascal-case :identifier)))

  (rule <expr> (naked) ::=
	(one-of :value <lambda> <string-literal> <array-literal> <object-literal> <regex-literal>
		<group>
		<field-ref> <field-ref-this> <field-ref-self>
		<funcall> <apply> <prototype-ref> <type-ref> <array-ref> <new> <call> <call-this> <call-self>
		<post++> <post--> <++> <--> <positive> <negative> <~> <!> <delete> <typeof> <void>
		<*> </> <%>
		<+> <->
		<<<> <>>> <>>>>
		<<> <>> <<=> <>=> <instanceof> <in>
		<==> <!=> <===> <!==>
		<bit-and>
		<bit-xor>
		<bit-or>
		<and>
		<or>
		<?>
		<set> <=> <+=> <-=> <*=> </=> <%=> <bit-and=> <bit-xor=> <bit-or=> <<<=> <>>=> <>>>=>
		<progn>))

  (rule <lambda> ::=
	"function"
	'open-paren-no-space (? <params>) 'close-paren
	'begin-block
	(* <stmt>)
	'end-block-no-new-line)

  (rule <string-literal> ::= '("\"" t nil) 'suppress-wrap :value '("\"" nil t nil))
  (rule <array-literal> ::= '("[" t nil) (* <expr> (list-separator 'comma)) '("]" nil t))

  (rule <object-literal> ::= "{" (* <prop> (list-separator 'comma)) "}")
  (rule <prop> ::= (one-of <string-literal> :id) '(":" nil t) <expr>)

  (rule <regex-literal> ::= "REGEX-LITERAL (TODO)")

  (precedence-rules <expr>

    ;; grouping
    ((rule <group> ::= <expr>))

    ;; accessors and new
    ((rule <field-ref> ::= <expr> '("." nil nil) <type-or-field> (* '("." nil nil) <type-or-field>))
     (rule <field-ref-this> ::= '("this." t nil) <type-or-field> (* '("." nil nil) <type-or-field>))
     (rule <field-ref-self> ::= '("self." t nil) <type-or-field> (* '("." nil nil) <type-or-field>))
     (rule <funcall> ::= 'open-paren <expr> 'close-paren '(".call" nil nil) <arg-list-with-nil>)
     (rule <apply> ::= 'open-paren <expr> 'close-paren '(".apply" nil nil) <arg-list>)
     (rule <prototype-ref> ::= (one-of <type-ref> <type-ref-internal>) '(".prototype" nil t))
     (rule <type-ref> ::= <type-ref-internal>)
     (rule <array-ref> ::= <expr> (* '("[" nil nil) <expr> '("]" nil t)))
     (rule <new> ::= "new" <type-expr> <arg-list>))

    ;; unary
    ((rule <post++> ::= <expr> '("++" nil t))
     (rule <post--> ::= <expr> '("--" nil t))
     (rule <++> ::= '("++" t nil) <expr>)
     (rule <--> ::= '("--" t nil) <expr>)
     (rule <positive> ::= '("+" t nil) <expr>)
     (rule <negative> ::= '("-" t nil) <expr>)
     (rule <~> ::= '("~" t nil) <expr>)
     (rule <!> ::= '("!" t nil) <expr>)
     (rule <delete> ::= "delete" <expr>)
     (rule <typeof> ::= "typeof" <expr>)
     (rule <void> ::= "void" <expr>))

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
     (rule <instanceof> ::= <expr> "instanceof" <type-expr>)
     (rule <in> ::= <expr> "in" <expr>))

    ;; equality
    ((rule <==> ::= <expr> "==" <expr>)
     (rule <!=> ::= <expr> "!=" <expr>)
     (rule <===> ::= <expr> "===" <expr>)
     (rule <!==> ::= <expr> "!==" <expr>))

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

  (rule <type-or-field> (naked) ::= (one-of <type-ref> :identifier))
  (rule <type-ref-internal> (naked) ::= 'pascal-case :identifier (* '("." nil nil) 'pascal-case :identifier))

  (rule <call> ::= <expr> '("." nil nil) :identifier <arg-list>)
  (rule <call-this> ::= '("this." nil nil) :identifier <arg-list>)
  (rule <call-self> ::= '("self." nil nil) :identifier <arg-list>)

  (rule <stmt> (naked) ::= (one-of <_> <//> <stmt-no-var-while-return> <var> <while> <return>))
  (rule <stmt-no-while> (naked) ::= (one-of <_> <//> <stmt-no-var-while-return> <var> <return>))
  (rule <stmt-no-var> (naked) ::= (one-of <_> <//> <stmt-no-var-while-return> <while> <return>))
  (rule <stmt-no-return> (naked) ::= (one-of <_> <//> <stmt-no-var-while-return> <while>))
  (rule <stmt-no-var-while-return> (naked) ::=
	(one-of (group (one-of <expr> <goto> <throw> <break> <continue>) 'semicolon)
		<function> <constructor> <block> <with> <if> <for> <for-each> <do> <switch> <try> <label>))

  (rule <var> ::= "var" :identifier (? "=" <expr>) 'semicolon)

  (rule <for.for-each.while.progn> <var> ::= :identifier (? "=" <expr>))

  (rule <_> ::= 'vertical-space)

  (rule <return> ::= "return" <expr> 'semicolon)
  (rule <goto> ::= "goto" 'pascal-case :label)
  (rule <throw> ::= "throw" <expr>)
  (rule <break> ::= "break" (? 'pascal-case :label))
  (rule <continue> ::= "continue" (? 'pascal-case :label))

  (rule <function> ::=
	'vertical-space
	"function" :name
	'open-paren-no-space (? <params>) 'close-paren
	'begin-block
	(* <stmt>)
	'end-block)

  (rule <constructor> ::=
	'vertical-space
	"function" 'pascal-case :name
	'open-paren-no-space (? <params>) 'close-paren
	'begin-block
	(* <stmt>)
	'end-block)

  (rule <block> ::=
	'vertical-space
	(one-of (group <stmt-no-var> (* <stmt-no-var>))
		(group 'begin-block (* <stmt>) 'end-block)))

  (rule <with> ::= "with" 'open-paren <expr> 'close-paren 'begin-block (* <stmt>) 'end-block)

  (rule <if.for.for-each.while.do.try> <block> ::= (* <stmt>))

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
	(one-of <progn> <var> <expr>) "in"
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
	(one-of (group (* <stmt-no-return>)
		       "break" 'semicolon)
		(* <stmt>))
	'decrease-indent)

  (rule <switch> <case> ::= 'vertical-space "case" <expr> <switch-case-body>)
  (rule <switch> <default> ::= 'vertical-space "default" <switch-case-body>)

  (rule <try> ::=
	"try" 'begin-block (* <stmt>)
	(* <catch>)
	(? <finally>)
	'end-block)

  (rule <try> <catch>  ::= 'end-block-no-new-line "catch" 'open-paren :name 'close-paren 'begin-block (* <stmt>))
  (rule <try> <finally> ::= 'end-block-no-new-line "finally" 'begin-block (* <stmt>))

  (rule <label> ::= 'vertical-space 'pascal-case :label '(":" nil t) 'new-line)

  ;; Make + and - polymorphic: both unary and n-ary.
  (macro <+> ::= (if (third +) `(,+) `((positive ,(second +)))))
  (macro <-> ::= (if (third -) `(,-) `((negative ,(second -)))))

  ;; cond: if/else if.../else.
  (macro <cond> ::=
	 (labels ((rec (remaining-clauses)
		    (let ((clause (first remaining-clauses)))
		      (if (member (first clause) '(t :true (and)) :test #'equal)
			  (second clause)
			  `(if ,(first clause)
			    ,(second clause)
			    ,@(awhen (rest remaining-clauses)
				     (list (rec it))))))))
	   (list (rec (rest cond)))))

  ;; and: Lisp-style multi-param 'and'
  (macro <and> ::=
	 (list
	  (aif (rest and)
	       (reduce #'(lambda (and-expr x)
			   (if and-expr
			       `(and ,and-expr ,x)
			       x))
		       it)
	       :true)))

  ;; Classes.
  (macro <class> ::=
	 (let* ((name (spath class :name))
		(container (spath class 'container))
		(name-expr `(type-ref ,@(rest container) ,name))
		(prototype-expr `(prototype-ref ,name-expr)))
	   `(,@(awhen (spath class 'constructor)
		      (if container
			  `((_) (set ,name-expr (lambda (params) ,@(rest it))))
			  `((_) (constructor ,name ,@(rest it)))))
	     ,@(awhen (spath class 'prototype)
		      `((_) (set ,prototype-expr (new ,@(rest it)))))
	     ,@(mappend #'(lambda (method)
			   `((_)
			     (set (field-ref ,prototype-expr ,(spath method :name))
			      (lambda ,@(get-zexp-rest method)))))
		       (spath class 'method*)))))

  ;; Thunks.
  (macro <thunk-call> ::=
	 `((funcall (lambda ,@(rest thunk-call))))))


(defun make-javascript-source-generator (&optional (file-name *standard-output*))
  (make-instance 'source-file-generator
		 :output-file file-name
		 :keyword-overrides '((:prototype "prototype")
				      (:void "void"))))


;;; Entry point.
(defun generate-javascript (module output-dir use-target-dir)
  (assert (spath module :name))
  (let ((filename (merge-pathnames (if use-target-dir output-dir (parse-namestring "."))
				   (parse-namestring
				    (string+ (symbol->pascal-string (spath module :name))
					     ".js")))))
    (progn
      (ensure-directories-exist filename :verbose t)
      (format t "~&Generating ~A...~%" (file-namestring filename))
      (with-open-file (out-file filename :direction :output
				         :if-exists :supersede)
	(let ((gen (make-javascript-source-generator out-file)))
	  (generate-sexp gen #'javascript-parse module))))))

(export 'generate-javascript)


;;; Generator used during interactive testing.
(defun test-parse (js)
  (prog1 nil
    (generate-sexp (make-javascript-source-generator *standard-output*)
		   #'javascript-parse
		   js)))


;;;; Test.
(define-constant +javascript-test+
  '(module :test
    (class :up-down-counter-sample (prototype :pst-node)
     (constructor
      (var :counter-value
	   (set (field-ref-this :counter-value)
		(call (new :integer-type) :create-value 1))))
     (method :create (params :dom-parent)
      (call (field-ref-this :horizontal-layout) :create :dom-parent)
      (set (field-ref-this :horizontal-layout :parent) :this)))
    (function :on-load
     (var :up-down-counter-sample (new :up-down-counter-sample))
     (call :up-down-counter-sample :create (field-ref :document :body)))))

;;; Test for hand execution.
(eval-when (:execute)
  (prog1 nil
    (generate-sexp (make-javascript-source-generator) #'javascript-parse +javascript-test+)))

;;; Real test.
(test "parse-javascript"
      '("// generated code: " pascal-case :test new-line vertical-space vertical-space
	"function" pascal-case :up-down-counter-sample open-paren-no-space close-paren
	begin-block "var" :counter-value "=" ("this." t nil) :counter-value "=" "new"
	pascal-case :integer-type open-paren-no-space close-paren ("." nil nil)
	:create-value open-paren-no-space 0 close-paren semicolon end-block
	vertical-space pascal-case :up-down-counter-sample (".prototype" nil t) "="
	"new" pascal-case :pst-node open-paren-no-space close-paren semicolon
	vertical-space pascal-case :up-down-counter-sample (".prototype" nil t)
	("." nil nil) :create "=" "function" open-paren-no-space :dom-parent
	close-paren begin-block ("this." t nil) :horizontal-layout ("." nil nil)
	:create open-paren-no-space :dom-parent close-paren semicolon ("this." t nil)
	:horizontal-layout ("." nil nil) :parent "=" :this semicolon
	end-block-no-new-line semicolon vertical-space "function" :on-load
	open-paren-no-space close-paren begin-block "var" :up-down-counter-sample "="
	"new" pascal-case :up-down-counter-sample open-paren-no-space close-paren
	semicolon :up-down-counter-sample ("." nil nil) :create open-paren-no-space
	:document ("." nil nil) :body close-paren semicolon end-block)
      (funcall (compose #'catharsis::parse-result-gen-list
			#'javascript-parse)
	       +javascript-test+))
