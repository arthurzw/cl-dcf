(in-package :com.google.catharsis)


;;; Parser infrastructure.

(defstruct parse-state
  form-stream
  zexp-name
  stack
  gen-list)


(defstruct parse-result
  form-stream
  gen-list)


(defun parse-state->parse-result (ps)
  "Creates a parse result from a parse state."
  (make-parse-result :form-stream (parse-state-form-stream ps)
		     :gen-list (parse-state-gen-list ps)))


(defun clean-parse-state (ps)
  "Creates a clean parse state based on the context of an existing parse state."
  (make-parse-state :form-stream (parse-state-form-stream ps)
		    :zexp-name (parse-state-zexp-name ps)
		    :stack (parse-state-stack ps)))


(defun parse-literal (literal parse-state)
  (make-parse-result :form-stream (parse-state-form-stream parse-state)
		     :gen-list (list literal)))


(defun parse-identifier (parse-state)
  (let* ((form-stream (parse-state-form-stream parse-state))
         (x (first form-stream))
         (xs (rest form-stream)))
    (when (or (stringp x)
              (numberp x)
              (keywordp x)
              (and (symbolp x) (casing-package-p x)))
	(make-parse-result :form-stream xs
                       :gen-list (list x))))) ; Can parse 'pascal-string, etc. here too.


(defun parse-zexp-name (parse-state)
  (awhen (parse-state-zexp-name parse-state)
    (make-parse-result :form-stream (parse-state-form-stream parse-state)
		       :gen-list (list it))))


(defun apply-parser (parse-state parser)
  (when (and parse-state parser)
    (let ((parse-result (funcall parser parse-state)))
      (when parse-result
	(make-parse-state :form-stream (parse-result-form-stream parse-result)
			  :zexp-name (parse-state-zexp-name parse-state)
			  :stack (parse-state-stack parse-state)
			  :gen-list (append (parse-state-gen-list parse-state)
					    (parse-result-gen-list parse-result)))))))


(defun sequence-parsers (parse-state &rest parsers)
  (reduce #'apply-parser parsers :initial-value parse-state))


(defun apply-best-parser (parse-state parser-list)
  "Applies the most applicable parser from the list. Returns (values parse-result parser)."
  (if (null parser-list)
      (parse-state->parse-result parse-state)
      (let* ((intermediate-results-list
	      ;; Triples: (index parser parse-result) for all parsers under consideration.
	      (zip (integer-list 1 (length parser-list))
		   parser-list
		   (mapcar #'(lambda (parser)
			       (when parser
				 (funcall parser (clean-parse-state parse-state))))
			   parser-list)))
	     (first-successful-entry
	      ;; First entry that parsed successfully.
	      (find-if #'identity
		       intermediate-results-list
		       :key #'third))
	     (first-entry-consumed-input
	      ;; First entry that parsed successfully and consumed some input.
	      (find-if #'(lambda (parse-result)
			   (when parse-result
			     (not (equal (parse-state-form-stream parse-state)
					 (parse-result-form-stream parse-result)))))
		       intermediate-results-list
		       :key #'third))
	     (chosen-entry
	      ;; We give preference to the parser that consumed some input, because
	      ;; a (? ...) parser will always succeed.
	      (aif first-entry-consumed-input it first-successful-entry)))
	(when chosen-entry	   ; Has one of the parsers succeeded?
	  (values (third chosen-entry)
		  (second chosen-entry))))))


;;;; Parser combinator generators.

(defun serial-parser (parse-form-list)
  `#'(lambda (parse-state)
       (aif (sequence-parsers parse-state ,@parse-form-list)
	    (parse-state->parse-result it))))


(defun one-of-parser (parse-form-list)
  `#'(lambda (parse-state)
       (apply-best-parser parse-state (list ,@parse-form-list))))


(defun unordered-parser-internal (parse-state parser-list result-list)
  (if (every #'null parser-list)
      (values result-list (parse-state-form-stream parse-state))
      (multiple-value-bind (parse-result chosen-parser)
	  (apply-best-parser parse-state parser-list)
	(when parse-result
	  (let* ((new-parser-result-zipped-list
		  ;; List of pairs: (parser result). The currently successful parser
		  ;; is replaced by a nil, and the current result is used.
		  (mapcar #'(lambda (parser result)
			      (if (eq parser chosen-parser)
				  (list nil parse-result)
				  (list parser result)))
			  parser-list
			  result-list))
		 ;; Split this up into separate lists.
		 (unzipped (unzip new-parser-result-zipped-list))
		 (new-parser-list (first unzipped))
		 (new-result-list (second unzipped)))
	    (unordered-parser-internal
	     (make-parse-state :form-stream (parse-result-form-stream parse-result)
			       :zexp-name (parse-state-zexp-name parse-state)
			       :stack (parse-state-stack parse-state))
	     new-parser-list
	     new-result-list))))))


(defun unordered-parser (parse-form-list)
  `#'(lambda (parse-state)
       (multiple-value-bind (final-results final-form-stream)
	   (unordered-parser-internal (clean-parse-state parse-state)
				      (list ,@parse-form-list)
				      (make-list ,(length parse-form-list)))
	 (when final-results
	   (make-parse-result :form-stream final-form-stream
			      :gen-list (apply #'append
					       (mapcar #'parse-result-gen-list
						       (remove nil final-results))))))))


(defun group-parser (parse-form-list)
  `#'(lambda (parse-state)
       (funcall ,(serial-parser parse-form-list)
		(clean-parse-state parse-state))))


(defun optional-parser (parse-form-list)
  `#'(lambda (parse-state)
       (aif (funcall ,(serial-parser parse-form-list)
		     (clean-parse-state parse-state))
	    it
	    (make-parse-result :form-stream (parse-state-form-stream parse-state)))))


(defun repeating-parser (parse-form-list list-separator)
  `#'(lambda (parse-state)
       (labels ((parse-next (,@(when list-separator `(first)) parse-state)
		  (aif (funcall ,(serial-parser parse-form-list)
				(clean-parse-state parse-state))
		       (parse-next
			,@(when list-separator `(nil))
			(make-parse-state :form-stream (parse-result-form-stream it)
					  :zexp-name (parse-state-zexp-name parse-state)
					  :stack (parse-state-stack parse-state)
					  :gen-list (append (parse-state-gen-list parse-state)
							    ,(when list-separator
								   `(unless first
								     (list ,@(rest list-separator))))
							    (parse-result-gen-list it))))
		       (parse-state->parse-result parse-state))))
	 (parse-next ,@(when list-separator `(t))
		     (clean-parse-state parse-state)))))


;;; Grammar rules and helper functions.

(defstruct macro form expand-fn-name body)

(defstruct rule
  composite-name ;; Name corresponding to all items in stack-patterns.
  forms          ;; List of tops of stack patterns.
  stack-patterns ;; List of stack patterns -- one of which must match the top of the form stack, e.g. (method interface).
  annotations    ;; Rule annotations, such as (zexp) or (naked).
  body)          ;; Rule body (a list).

(defstruct rule-component parse-form)


(defvar *grammar-name* nil
  "Dynamic variable holding the name of the grammar being generated.")


(defun is-rule (sym)
  "Determines whether sym is a rule symbol, enclosed in angle brackets."
  (when (symbolp sym)
    (let ((sym-name (symbol-name sym)))
      (and (char= (char sym-name 0) #\<)
	   (char= (last-char sym-name) #\>)))))


(defun rule-name (sym)
  "Retrieves the symbol from a rule name, e.g. <rule> maps to rule."
  (when (is-rule sym)
    (rename-symbol sym #'(lambda (sym-name)
			   (subseq sym-name 1 (1- (length sym-name)))))))


(defun or-rule->sym-list (sym)
  "Translates an encoded list such as rule1.rule2 into (rule1 rule2)."
  (mapcar (curry #'gen-symbol (symbol-package sym))
	  (split-sequence #\. (symbol-name sym))))


(defun combine-head (head)
  "Produces the combinatorial expansion of head rules."
  (if (rest head)
      (mappend #'(lambda (head-item)
		  (mapcar (curry #'cons head-item)
			  (combine-head (rest head))))
	      (or-rule->sym-list (first head)))
      (mapcar #'list (or-rule->sym-list (first head)))))

(test "combine-head"
      '((a1 b c1) (a1 b c2) (a1 b c3) (a2 b c1) (a2 b c2) (a2 b c3))
      (combine-head '(a1.a2 b c1.c2.c3)))


(defun parse-macro (macro-rest)
  "Parses a macro at the head of macro-rest into a macro structure."
  (let* ((macro (first macro-rest))
	 (form (rule-name (second macro))))
    (make-macro :form form
		:expand-fn-name (macro-fn-name form
					       (count form macro-rest
						      :key (compose #'rule-name #'second)))
		:body (drop 3 macro))))


(defun parse-rule (rule)
  "Parses a rule into a rule structure."
  (let* ((separator-pos (position ::= rule))
	 (head+annotations (subseq rule 1 separator-pos))
	 (body (subseq rule (1+ separator-pos)))
	 (head (remove-if-not #'is-rule head+annotations))
	 (head-combinations (combine-head (mapcar #'rule-name head)))
	 (annotations (remove-if #'is-rule head+annotations))
	 (package (symbol-package (first head+annotations))))
    (when (eq (first rule) 'rule)
      (make-rule :composite-name (apply (curry #'gen-symbol package)
					(apply #'append
					       (add-separators '(!)
							       head-combinations)))
		 :forms (or-rule->sym-list (rule-name (first (last head))))
		 :stack-patterns (mapcar #'reverse head-combinations)
		 :annotations annotations
		 :body body))))


(defun macro-fn-name (macro-name id)
  "Name of the macroexpanding function for the supplied macro."
  (gen-symbol nil *grammar-name* :expand macro-name id))


(defun rule-fn-name (rule type)
  "Name of the function that processes the supplied rule.
   type determines the function type. It should be one of: parse, gen"
  (gen-symbol nil
	      *grammar-name*
	      type
	      (rule-composite-name rule)))


(defun parse-form-list (rule-component-list)
  (remove nil
	  (mapcar (compose #'rule-component-parse-form
			   #'parse-rule-component)
		  rule-component-list)))


(defun parse-rule-component (rule-component)
  "Parses the supplied rule-component, producting gen code forms."
  (cond ((stringp rule-component) ;; String literal -> emit it.
	 (make-rule-component :parse-form `(curry #'parse-literal ,rule-component)))

	((eq rule-component :name) ;; :name -> zexp name.
	 (make-rule-component :parse-form `#'parse-zexp-name))

	((keywordp rule-component) ;; :keyword -> identifier.
	 (make-rule-component :parse-form `#'parse-identifier))

	((is-rule rule-component) ;; <rule>
	 (make-rule-component :parse-form `(curry #',(gen-symbol nil *grammar-name* :parse-internal)
					    ',(rule-name rule-component))))

	((listp rule-component)
	 (case (first rule-component)

	   ;; '...
	   (quote
	    (if (atom (second rule-component))

		;; 'control-atom -> emit-<control-atom>
		(make-rule-component :parse-form `(curry #'parse-literal ',(second rule-component)))

		;; '(token ...) -> emit-token token ...
		(make-rule-component :parse-form `(curry #'parse-literal ',@(rest rule-component)))))

	   (one-of ;; (one-of ...) -> disjunction.
	    (make-rule-component :parse-form (one-of-parser (parse-form-list (rest rule-component)))))

	   (unordered ;; (unordered ...) -> unordered group.
	    (make-rule-component :parse-form (unordered-parser (parse-form-list (rest rule-component)))))

	   (group ;; (group ...) -> simple grouping.
	    (make-rule-component :parse-form (serial-parser (parse-form-list (rest rule-component)))))

	   (? ;; (? ...) -> optional component.
	    (make-rule-component :parse-form (optional-parser (parse-form-list (rest rule-component)))))

	   (* ;; (* ... (list-separator...))
	    (make-rule-component :parse-form (repeating-parser
					      (parse-form-list
					       (remove-if #'(lambda (x)
							      (and (listp x)
								   (eq (first x) 'list-separator)))
							  (rest rule-component)))
					      (spath rule-component 'list-separator))))))

	(t (make-rule-component))))


(defun macro-fn (macro)
  "Generates code for a macroexpansion routine.
   The output of this function is used in the context established by def-grammar."
  `(defun ,(macro-expand-fn-name macro) (,(macro-form macro))
     (declare (ignorable ,(macro-form macro)))
     ,@(macro-body macro)))


(defun has-name (rule)
  (or (find '(zexp) (rule-annotations rule) :test #'equal)
      (findtree :name (rule-body rule))))


(defun parse-fn (rule)
  "Generates code for a parse-rule routine corresponding to the rule.
   The output of this function is used in the context established by def-grammar."
  `(defun ,(rule-fn-name rule :parse) (parse-state)

    ,(cond ((spath (rule-annotations rule) 'naked)
            `(funcall ,(serial-parser (parse-form-list (rule-body rule)))
                      (make-parse-state :form-stream (parse-state-form-stream parse-state)
                                        :zexp-name (parse-state-zexp-name parse-state)
                                        :stack (parse-state-stack parse-state))))
           (t
            `(let* ((form-stream (parse-state-form-stream parse-state))
                    (x (expand-macros (first form-stream) macro-table))
                    (xs (rest form-stream)))
               (when (and (listp x)
                          (find (first x) ',(rule-forms rule)))
                 (awhen (funcall ,(serial-parser (parse-form-list (rule-body rule)))
                                 ,(if (has-name rule)
                                      `(make-parse-state
                                        :form-stream  (append (awhen (get-zexp-value x) (list it))
                                                              (get-zexp-rest x))
                                        :zexp-name (get-zexp-name x)
                                        :stack (cons (first x)
                                                     (parse-state-stack parse-state)))
                                      `(make-parse-state
                                        :form-stream (rest x)
                                        :stack (cons (first x)
                                                     (parse-state-stack parse-state)))))
                   (unless (parse-result-form-stream it)
                     (make-parse-result :form-stream xs
                                        :gen-list (parse-result-gen-list it))))))))))


(defun build-macro-table (macro-form-expand-fn-name-list)
  (let ((mt (make-hash-table)))
    (dolist (m macro-form-expand-fn-name-list)
      (destructuring-bind (form expand-fn-name) m
        (append1f (gethash form mt) expand-fn-name)))
    mt))

(test "build-macro-table"
      '((foo (expand-foo-1))
        (bar (expand-bar-1 expand-bar-2)))
      (hashtable->list
       (build-macro-table '((foo expand-foo-1)
                            (bar expand-bar-1)
                            (bar expand-bar-2)))))


(defun expand-macros (form macro-table &optional excluded-rules)
  (flet ((expand-form (x)
           (aif (and (listp x)
                     (first (set-difference (gethash (first x) macro-table)
                                            excluded-rules)))
                (expand-macros (funcall (symbol-function it) x)
                               macro-table
                               (cons it excluded-rules))
                :not-a-macro)))
    (if (listp form)
        (reduce #'(lambda (x macroexpansion)
                    (aif (expand-form x)
                         (if (eq it :not-a-macro)
                             (cons x macroexpansion)
                             (append it macroexpansion))
                         macroexpansion))
                form
                :from-end t
                :initial-value nil)
        form)))

(test "expand-macros"
      '(container (class :baz (prop :bar)) (interface :foo))
      (with-defuns ((expand-class-1 (class)
                                    (append (list class) '((interface :foo))))
                    (expand-class-2 (class)
                                    (list (append class '((prop :bar)))))
                    (expand-dead-construct (dead-construct)
                                           (declare (ignore dead-construct))
                                           nil)
                    (expand-prop-1 (prop)
                                   (append '((doc)) (list prop))))
        (expand-macros '(container (class :baz) (dead-construct))
                       (build-macro-table '((class expand-class-1)
                                            (class expand-class-2)
                                            (dead-construct expand-dead-construct)
                                            (prop expand-prop-1))))))


(defun build-dte-list (parsed-rules)
  (mappend #'(lambda (rule)
	       (mapcar #'(lambda (stack-pattern)
			   (list stack-pattern (rule-fn-name rule :parse)))
		       (rule-stack-patterns rule)))
	   parsed-rules))


(defun build-dispatch-table (dte-list)
  (bulk-insert-trie dte-list (make-instance 'trie)))


(defun lookup-dispatch-table (stack dt)
  (when (and stack dt)
    (lookup-prefix-in-trie stack dt)))


(test "dispatch-table"
      '(parse-r1-r2-r4-!-r1-r3-r4 parse-r1)
      (let ((dt (build-dispatch-table
		 (build-dte-list
		  (mapcar #'parse-rule
			  '((rule <r1> <r2.r3> <r4> ::= "r1-r2.r3-r4")
			    (rule <r1> ::= "r1")))))))
	(list (lookup-dispatch-table '(r4 r2 r1) dt)
	      (lookup-dispatch-table '(r1) dt))))


(defmacro def-grammar (name &body body)
  "Defines a grammar."
  (let* ((*grammar-name* name)
         (macroexpanded-body (mapcar #'macroexpand body))
         (macros (spath macroexpanded-body 'macro*))
         (parsed-macros (maplist #'parse-macro macros))
         (macro-list (zip (mapcar #'macro-form parsed-macros)
                          (mapcar #'macro-expand-fn-name parsed-macros)))
         (rules (append (spath macroexpanded-body 'rule*)
                        (spath macroexpanded-body '(progn* rule*))))
         (parsed-rules (mapcar #'parse-rule rules)))
    `(progn
       (let* ((*grammar-name* ',name)
              (macro-table (build-macro-table ',macro-list))
              (dispatch-table (build-dispatch-table ',(build-dte-list parsed-rules))))
         ,@(mapcar #'macro-fn parsed-macros)
         ,@(mapcar #'parse-fn parsed-rules)
         (defun ,(gen-symbol nil *grammar-name* :parse-internal) (expected-form parse-state)
           (let* ((stack (parse-state-stack parse-state))
                  (new-stack (cons expected-form stack))
                  (rule-fn (lookup-dispatch-table new-stack dispatch-table)))
             (if rule-fn
                 (funcall rule-fn parse-state)
                 (error "No matching rule."))))
         (defun ,(gen-symbol nil *grammar-name* :parse) (form)
           (,(gen-symbol nil *grammar-name* :parse-internal)
             (first form)
             (make-parse-state :form-stream (expand-macros (list form) macro-table))))))))

(export 'def-grammar)
(dolist (sym '(one-of  unordered  group  ?  *  list-separator  macro  rule  naked  zexp))
  (export sym))


(defmacro nullary-rule (form)
  `(rule ,form ::=
    ,(string-downcase (rule-name form))))

(export 'nullary-rule)


(defun rule-body-with-assoc (assoc assoc-rule-name equal-precedence-form-list body)
  (funcall (if (eq assoc :right) #'identity #'reverse)
   (cdr
    (reduce #'(lambda (rule-seen-new-body body-item)
		(let ((rule-seen (car rule-seen-new-body))
		      (new-body (cdr rule-seen-new-body)))
		  (if (eq body-item assoc-rule-name)
		      (cons t (cons (if rule-seen
					`(one-of (group
						  'open-paren
						  (one-of ,@equal-precedence-form-list)
						  'close-paren)
					  ,body-item)
					body-item)
				    new-body))
		      (cons rule-seen (cons body-item new-body)))))
	    (if (eq assoc :right) (reverse body) body)
	    :initial-value (cons nil nil)))))

(test "rule-body-with-assoc"
      '(("<" <expr> "-" (one-of (group 'open-paren (one-of <+> <->) 'close-paren) <expr>) ">")
	("<" (one-of (group 'open-paren (one-of <+> <->) 'close-paren) <expr>) "-" <expr> ">")
	("apply" <expr> <arg-list>))
      (list (rule-body-with-assoc :left '<expr> '(<+> <->) '("<" <expr> "-" <expr> ">"))
	        (rule-body-with-assoc :right '<expr> '(<+> <->) '("<" <expr> "-" <expr> ">"))
	        (rule-body-with-assoc :left '<expr> '(<apply>) '("apply" <expr> <arg-list>))))


(defun precedence-rule (form assoc-rule-name assoc equal-precedence-form-list higher-precedence-form-list body)
  "Produces a set of rules for the given form: one for each stack containing an operator
   of a higher precedence, and one for all other stacks."
  `(,(when higher-precedence-form-list
	   `(rule
	     ,(intern (apply #'string+
			     (append (list "<")
				     (add-separators "."
						     (mapcar (compose #'symbol-name #'rule-name)
							     higher-precedence-form-list))
				     (list ">")))
		      (symbol-package form))
	     ,form ::= 'open-paren ,@body 'close-paren))
    (rule ,form ::= ,@(rule-body-with-assoc assoc assoc-rule-name equal-precedence-form-list body))))


(defun equal-precedence-forms (form group-list)
  "Retrieves the list of all forms in group-list with precedence higher than form."
  (mapcar #'second
	  (find-if #'(lambda (x) (find form x :key #'second))
		   group-list)))


(defun higher-precedence-forms (form group-list)
  "Retrieves the list of all forms in group-list with precedence higher than form."
  (mapcar #'second
	  (apply #'append
		 (rest (member-if #'(lambda (x) (find form x :key #'second))
				  (reverse group-list))))))

(test "equal-and-higher-precedence-forms"
      '((<+> <->) (<*> </>))
      (let ((rules '(((rule <*> ::= "*")
		      (rule </> ::= "/"))
		     ((rule <+> ::= "+")
		      (rule <-> ::= "-"))
		     ((rule <x> ::= "x")))))
	(list (equal-precedence-forms '<+> rules)
	      (higher-precedence-forms '<+> rules))))


(defmacro precedence-rules (name &body group-list)
  `(progn
    ,@(mappend #'(lambda (rule)
		   (let ((parsed-rule (parse-rule rule))
			 (form (second rule)))
		     (assert (= (length (rule-forms parsed-rule)) 1))
		     (assert (= (length (rule-stack-patterns parsed-rule)) 1))
		     (precedence-rule
			 form
		         name
			 (or (spath (rule-annotations parsed-rule) '(assoc :name)) :left)
			 (equal-precedence-forms form group-list)
			 (higher-precedence-forms form group-list)
			 (rule-body parsed-rule))))
	       (apply #'append group-list))))

(export 'precedence-rules)


(defvar *print-bad-sexp* nil)
(export '*print-bad-sexp*)

(defun generate-sexp (gen parse-fn form)
  (aif (funcall parse-fn form)
       (apply #'emit-list gen (parse-result-gen-list it))
       (progn (when *print-bad-sexp* (print form))
	      (error "Parse failed."))))

(export 'generate-sexp)

#|

TODO: commented out due to loader ordering issues.

(def-grammar test-unordered-parser
  (rule <constructor> ::=
	'vertical-space
	(unordered (group 'pascal-case :name))
	'begin-block
	'end-block))

;;; Ensures that 'unordered' requires all clauses to succeed.
;;; In this case, there's no zexp name, so unordered should fail.
(test "unordered-parser-1"
      nil
      (funcall 'test-unordered-parser-parse '(constructor)))

|#
