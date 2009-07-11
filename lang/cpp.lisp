(in-package :com.google.catharsis.cpp)

(def-grammar cpp

  (rule <definition> (naked) ::= (one-of <_> <//> <define> <ifndef> <std-include> <include> <define> <macro-call>
					 <namespace> <using> <class-forward> <stmt>
					 <class> <field> <method> <constructor> <destructor> <var>))

  (rule <_> ::= 'vertical-space)
  (rule <//> ::= "//" (* :comment-string) 'new-line)

  (rule <ifndef> ::=
	"#ifndef" 'upper-case :name 'new-line
	(* <definition>)
	'vertical-space
	"#endif //" 'upper-case :name 'new-line)

  (rule <std-include> ::= '("#include <" t nil) :filename '(">" nil t) 'new-line)
  (rule <include> ::= '("#include \"" t nil) :filename '("\"" nil t) 'new-line)
  (rule <define> ::= "#define" 'upper-case :name 'suppress-wrap (? :definition) 'new-line) ;; TODO: \-prefixed line breaks (new source-gen feature).

  (rule <class> <public.protected.private> <macro-call> ::=
        'upper-case :macro-name <arg-list> 'semicolon 'new-line)
  (rule <macro-call> ::= 'upper-case :macro-name <arg-list>)

  (rule <namespace> ::= "namespace" 'lower-case :name 'begin-block 'decrease-indent
	                (* <definition>)
                    'vertical-space
                    'increase-indent 'end-block-no-new-line "// namespace" 'lower-case :name 'new-line)

  (rule <using> ::= "using" :namespace (* '("::" nil nil) (one-of :namespace <class-ref>)) 'semicolon 'new-line)

  (rule <class-forward> ::=
        "class" (* (one-of <class-ref>
                           (group 'pascal-case :class-name))
                   (list-separator '("::" nil nil)))
        'semicolon 'new-line)

  (rule <friend> ::=
        "friend class" (* (one-of <class-ref>
                                   (group 'pascal-case :class-name))
                           (list-separator '("::" nil nil)))
        'semicolon 'new-line)

  (nullary-rule <virtual>)
  (nullary-rule <abstract>)
  (nullary-rule <explicit>)
  (nullary-rule <public>)
  (nullary-rule <protected>)
  (nullary-rule <private>)
  (nullary-rule <static>)
  (nullary-rule <const>)
  (nullary-rule <volatile>)
  (nullary-rule <extern>)

  (rule <struct> ::=
	'vertical-space "struct" 'pascal-case :name
	'begin-block
	(* <field>)
	'end-block-no-new-line 'semicolon 'new-line)

  (rule <struct> <field> ::=
	(unordered (? <static>)
               <type>
               :name)
	'semicolon)

  (rule <class> ::=
	'vertical-space "class"
	(* (one-of <class-ref> (group 'pascal-case :class-name)) (list-separator '("::" nil nil)))
	(? (group ":" <inherit> (* 'comma <inherit>)))
	'begin-block
	(* (one-of <public> <protected> <private>))
	'end-block-no-new-line 'semicolon 'new-line)

  (rule <class> <inherit> ::=
        (unordered (? <abstract>)
                   (one-of <public> <protected> <private>)
                   (* (one-of <class-ref> <ns-ref> (group 'pascal-case :base-class-name))
                      (list-separator '("::" nil nil))))) ;; TODO: -> <class-name> (naked)

  (rule <class> <public> ::= 'decrease-indent 'vertical-space " public:" 'increase-indent 'new-line <class-contents>)
  (rule <class> <protected> ::= 'decrease-indent 'vertical-space " protected:" 'increase-indent 'new-line <class-contents>)
  (rule <class> <private> ::= 'decrease-indent 'vertical-space " private:" 'increase-indent 'new-line <class-contents>)

  (rule <class-contents> (naked) ::= (* (one-of <//>
                                                <_>
                                                <struct>
                                                <class>
                                                <class-forward>
                                                <friend>
                                                <field>
                                                <method>
                                                <constructor>
                                                <destructor>
                                                <macro-call>)))

  (rule <class> <public> <field> ::=
	'new-line
	(unordered (? <static>)
               <type>
               'lower-case :name)
	'semicolon)

  (rule <class> <protected.private> <field> ::=
	'new-line
	(unordered (? <static>)
               <type>
               'lower-case_ :name)
	'semicolon)

  (rule <field> ::=
	'vertical-space
	(unordered <type>
	           (* (one-of <class-ref> (group 'lower-case :field)) (list-separator '("::" nil nil)))
               (? (one-of <init> <constructor-call>))
               'semicolon
               'new-line))

  (rule <class> <public.protected.private> <method> ::=
        'new-line
        (unordered (? <static>)
                   (? <virtual>)
                   (one-of <return-type> "void")
                   (group 'pascal-case :name <param-list>)
                   (? <const>)
                   (? <volatile>))
        <class-member-fn-body>)

  (rule <method> ::=
	'vertical-space
	(unordered (one-of <return-type> "void")
               (* (one-of <class-ref> (group 'pascal-case :method)) (list-separator '("::" nil nil)))
               <param-list>
               (? <const>)
               (? <volatile>))
	'begin-block
	(* <stmt>)
	'end-block)

  (rule <class> <public.protected.private> <constructor> ::=
	'new-line
	(unordered (? <virtual>)
               (? <explicit>)
	           (group 'pascal-case :name)
               <param-list>
               (? ":" <init> (* 'comma <init>)))
	<class-member-fn-body>)

  (rule <constructor> ::=
	'vertical-space
	(unordered (* (one-of <class-ref> (group 'pascal-case :class-name)) (list-separator '("::" nil nil)))
               <param-list>
               (? ":" <init> (* 'comma <init>)))
	'begin-block
	(* <stmt>)
	'end-block)

  (rule <class> <public.protected.private> <destructor> ::=
	'new-line
	(unordered (? <virtual>)
		   (group '("~" t nil) 'pascal-case :name 'open-paren-no-space 'close-paren))
	<class-member-fn-body>)

  (rule <destructor> ::=
	'vertical-space
	(* (one-of <class-ref>
               (group '("~" t nil) 'pascal-case :class-name 'open-paren-no-space 'close-paren))
	   (list-separator '("::" nil nil)))
	'begin-block
	(* <stmt>)
	'end-block)

  (rule <class-member-fn-body> (naked) ::= (one-of <null-body>
                                                   (group 'begin-block <stmt> (* <stmt>) 'end-block)
                                                   'semicolon))

  (rule <constructor> <init> ::=
        (one-of :class-name (* <class-ref> (list-separator '("::" nil nil))))
        <arg-list>)

  (rule <init> ::= "=" <expr>)
  (rule <constructor-call> ::= <arg-list>)

  (rule <type> ::= <type-expr>)
  (rule <return-type> ::= <type-expr>)
  (rule <type-annotation> (naked) ::= (unordered (? <const>) (? <volatile)))

  (rule <type-expr> (naked) ::=
        (one-of (group 'pascal-case :type-name (? <template>))
                (group <class-ref> (? <template>))
                <ptr> <ref> <ns-ref> <field-ref-type>)

        <type-annotation>)

  (rule <field-ref-type> ::=
	(* <class-ref> '("::" nil nil)) :field-name)

  (rule <template> ::= '("<" nil nil) (* 'pascal-case <type-expr> (list-separator 'comma)) '(">" nil t))
  (rule <ptr> ::= <type-expr> '("*" nil t) <type-annotation>)
  (rule <ref> ::= <type-expr> '("&" nil t) <type-annotation>)
  (rule <ns-ref> ::= 'lower-case :ns)

  (rule <param-list> (naked) ::=
	'open-paren-no-space
	(* <param> (list-separator 'comma))
	'close-paren)

  (rule <param> ::= <type> (? (group 'lower-case :name)))

  (rule <arg-list> (naked) ::=
	'open-paren-no-space
	(* <expr> (list-separator 'comma))
	'close-paren)

  (rule <expr> (naked) ::= (one-of (group 'lower-case :value)
                                   <char-literal> <long-char-literal> <string-literal> <long-string-literal>
                                   <array-literal> <class-ref> <class-or-field>
                                   <scope-ref>
                                   <group>
                                   <call> <call-method> <call-static> <call-ptr> <call-this> <call-template> <macro-call>
                                   <post++> <post--> <array-ref> <field-ref> <field-ref-ptr> <field-ref-this> <field-ref-static>
                                   <++> <--> <positive> <negative> <~> <!> <addr> <deref> <new> <sizeof> <cast>
                                   <member-ref> <member-ref-ptr>
                                   <*> </> <%>
                                   <+> <->
                                   <<<> <>>>
                                   <<> <>> <<=> <>=>
                                   <==> <!=>
                                   <bit-and>
                                   <bit-xor>
                                   <bit-or>
                                   <and>
                                   <or>
                                   <?>
                                   <set> <=> <+=> <-=> <*=> </=> <%=> <bit-and=> <bit-xor=> <bit-or=> <<<=> <>>=>
                                   <progn>))

  (rule <char-literal> ::= '("'" t nil) 'suppress-wrap :value '("'" nil t nil))
  (rule <long-char-literal> ::= '("L'" t nil) 'suppress-wrap :value '("'" nil t nil))
  (rule <string-literal> ::= '("\"" t nil) 'suppress-wrap :value '("\"" nil t nil))
  (rule <long-string-literal> ::= '("L\"" t nil) 'suppress-wrap :value '("\"" nil t nil))
  (rule <array-literal> ::= "{" (* <expr> (list-separator 'comma)) "}")
  (rule <class-ref> ::= <type-expr> (* '("::" nil nil) <type-expr>))
  (rule <class-or-field> (naked) ::= (one-of (group 'lower-case :identifier) <class-ref>))

  (precedence-rules <expr>

    ;; scope
    ((rule <scope-ref> ::= <expr> (* '("::" nil nil) <expr>)))

    ;; grouping
    ((rule <group> ::= <expr>))

    ;; postfix and accessors
    ((rule <post++> ::= <expr> '("++" nil t))
     (rule <post--> ::= <expr> '("--" nil t))
     (rule <array-ref> ::= <expr> (* '("[" nil nil) <expr> '("]" nil t)))
     (rule <field-ref> ::= <expr> '("." nil nil) <class-or-field> (* '("." nil nil) <class-or-field>))
     (rule <field-ref-ptr> ::= <expr> '("->" nil nil) <class-or-field> (* '("->" nil nil) <class-or-field>))
     (rule <field-ref-this> ::= '("this->" t nil) <class-or-field>)
     (rule <field-ref-static> ::= (* <class-ref> '("::" nil nil)) <class-or-field>))

    ;; unary
    ((rule <++> (assoc :right) ::= '("++" t nil) <expr>)
     (rule <--> (assoc :right) ::= '("--" t nil) <expr>)
     (rule <positive> (assoc :right) ::= '("+" t nil) <expr>)
     (rule <negative> (assoc :right) ::= '("-" t nil) <expr>)
     (rule <~> (assoc :right) ::= '("~" t nil) <expr>)
     (rule <!> (assoc :right) ::= '("!" t nil) <expr>)
     (rule <addr> (assoc :right) ::= '("&" t nil) <expr>)
     (rule <deref> (assoc :right) ::= '("*" t nil) <expr>)
     (rule <new> (assoc :right) ::= "new" <type-expr> <arg-list>)
     (rule <sizeof> (assoc :right) ::= "sizeof" <expr>)
     (rule <cast> (assoc :right) ::= 'open-paren <type-expr> 'close-paren <expr>))

    ;; member pointer selectors
    ((rule <member-ref> ::= <expr> '(".*" nil nil) <class-or-field>)
     (rule <member-ref-ptr> ::= <expr> '("->*" nil nil) <class-or-field>))

    ;; multiplicative
    ((rule <*> ::= <expr> "*" <expr> (* "*" <expr>))
     (rule </> ::= <expr> "/" <expr>)
     (rule <%> ::= <expr> "%" <expr>))

    ;; additive
    ((rule <+> ::= <expr> "+" <expr> (* "+" <expr>))
     (rule <-> ::= <expr> "-" <expr> (* "-" <expr>)))

    ;; shift
    ((rule <<<> ::= <expr> "<<" <expr>)
     (rule <>>> ::= <expr> ">>" <expr>))

    ;; relational
    ((rule <<> ::= <expr> "<" <expr>)
     (rule <>> ::= <expr> ">" <expr>)
     (rule <<=> ::= <expr> "<=" <expr>)
     (rule <>=> ::= <expr> ">=" <expr>))

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
     (rule <>>=> (assoc :right) ::= <expr> ">>=" <expr>))

    ;; comma
    ((rule <progn> ::= <expr> (* 'comma <expr>))))

  (rule <call> ::= (* <ns-ref> '("::" nil nil)) 'pascal-case :identifier <arg-list>)
  (rule <call-method> ::= <expr> '("." nil nil) 'pascal-case :identifier <arg-list>)
  (rule <call-static> ::=
        (one-of (group 'pascal-case :identifier) <class-ref>)
        '("::" nil nil) 'pascal-case :identifier <arg-list>)
  (rule <call-ptr> ::= <expr> '("->" nil nil) 'pascal-case :identifier <arg-list>)
  (rule <call-this> ::= '("this->" t nil) 'pascal-case :identifier <arg-list>)
  (rule <call-template> ::=
        (one-of (group 'pascal-case :identifier) <class-ref>)
        '("::" nil nil) 'pascal-case :identifier
        <template>
        <arg-list>)

  (rule <null-body> ::= "{}")

  ;; Statements.

  (rule <stmt> (naked) ::= (one-of <_> <//> <stmt-no-var-while> <var> <while>))
  (rule <stmt-no-while> (naked) ::= (one-of <_> <//> <stmt-no-var-while> <var>))
  (rule <stmt-no-var> (naked) ::= (one-of <_> <//> <stmt-no-var-while> <while>))
  (rule <stmt-no-var-while> (naked) ::=
	(one-of (group (one-of <expr> <return> <goto> <throw> <break> <continue> <delete>) 'semicolon)
		<block> <if> <for> <do> <switch> <try> <label>))

  (rule <var> ::=
	(unordered (? <const>) (? <volatile>) (? <static>) (? <extern>) <type>)
	'lower-case :name
	(? (one-of <init> <constructor-call>))
	'semicolon)

  (rule <for.while.progn> <var> ::= <type> :name (? <init>))

  (rule <_> ::= 'vertical-space)

  (rule <return> ::= "return" <expr>)
  (rule <goto> ::= "goto" 'pascal-case :label)
  (rule <throw> ::= "throw" <expr>)
  (rule <break> ::= "break" (? 'pascal-case :label))
  (rule <continue> ::= "continue" (? 'pascal-case :label))
  (rule <delete> ::= "delete" <expr>)

  (rule <block> ::=
	(one-of (group <stmt-no-var> (* <stmt-no-var>))
		(group 'begin-block (* <stmt>) 'end-block)))

  (rule <if.for.while.do.try> <block> ::= (* <stmt>))

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

    ;; Make +, -, and * polymorphic: both unary and n-ary.
  (macro <+> ::= (if (third +) `(,+) `((positive ,(second +)))))
  (macro <-> ::= (if (third -) `(,-) `((negative ,(second -)))))
  (macro <*> ::= (if (third *) `(,*) `((deref ,(second *)))))

  ;; cond: if/else if.../else.
  (macro <cond> ::=
	 (labels ((rec (remaining-clauses)
                (let ((clause (first remaining-clauses)))
                  (if (eq (first clause) 't)
                      (second clause)
                      `(cpp:if ,(first clause)
                               ,(second clause)
                               ,@(awhen (rest remaining-clauses)
                                        (list (rec it))))))))
	   (if (rest cond)
	       (list (rec (rest cond)))
	       `((cpp:block))))))


(defun make-cpp-source-generator (&optional (file *standard-output*))
  (make-instance 'source-file-generator
		 :output-file file
		 :keyword-overrides '((:void "void")
                              (:byte "byte")
                              (:short "short")
                              (:int "int")
                              (:long "long")
                              (:float "float")
                              (:double "double")
                              (:char "char")
                              (:bool "bool")
                              (:true "true")
                              (:false "false")
                              (:null "NULL")
                              (:string "string")
                              (:wstring "wstring")
                              (:scoped-ptr "scoped_ptr"))))

;;;; Test.
(define-constant +cpp-test+
  '((// "Copyright 2007 Google Inc. All Rights Reserved.")
    (_)
    (ifndef :gdata-data-extensions-who-h
     (std-include :vector)
     (include "util/stl_util-inl.h")
     (include "data/extensions/who.h")
     (namespace :gdata
      (namespace :data
		 (namespace :extensions
			    (using :gdata :data pascal-case::extension-description)
			    (class-forward :entry-link)
			    (class :who (inherit :base-1 (public)) (inherit :base-2 (protected) (abstract))
				   (public
				    (struct :rel
					    (field :k-event-attendee (type :wstring (const)) (static))
					    (field :k-event-organizer (type :wstring (const)) (static)))
				    (constructor :who (param :param-1 (type :int)) (init :f1 (long-string-literal "abc") 1) (virtual))
				    (destructor :who (virtual) (null-body)))
				   (protected
				    (field :f1 (type :int))
				    (field :f2 (type (ptr :int)))
				    (field :f3 (type (ptr :int (const))))
				    (field :f4 (type (ptr :int) (const)))
				    (field :f5 (type (ptr :int (const)) (const)))
				    (field :f5 (static) (type (ref :big-structure-type (const)))))
				   (private
				    (class-forward :handler)
				    (method :foo (return-type (ptr :xml-parser :element-handler))
					    (param (type :scoped-ptr (template :entry-link))) (virtual) (const))
				    (macro-call :disallow-evil-constructors pascal-case::who)))))))))

;;; Test for hand execution.
(eval-when (:execute)
  (prog1 nil
    (dolist (cpp-form +cpp-test+)
      (generate-sexp (make-cpp-source-generator) #'cpp-parse cpp-form)
      (format t "~%"))))


(defun generate-file-internal (program &key (file *standard-output*))
  (let ((gen (make-cpp-source-generator file)))
    (dolist (cpp-form program)
      (generate-sexp gen #'cpp-parse cpp-form))
    (emit-list gen 'vertical-space)))


(defun generate-file (program filename &key output-dir)
  (if output-dir
      (progn
        (format t "Generating ~A...~%" filename)
        (with-open-file (f (concatenate 'string output-dir "/" filename)
                           :direction :output
                           :if-exists :supersede)
          (generate-file-internal program :file f)))
      (progn
        (format t "~&~%~%>>>>>>>>>> ~A~%~%" filename)
        (generate-file-internal program))))

(export 'generate-file)
