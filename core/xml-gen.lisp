(in-package :com.google.catharsis)

;;; Package containing built-in constructs (see the export list).
(defpackage :com.google.catharsis.xml
  (:nicknames :xml)
  (:export :text :cdata :ns :lang))


;;; Test packages
(defpackage :com.google.catharsis.xml.ns1 (:nicknames :ns1))
(defpackage :com.google.catharsis.xml.ns2 (:nicknames :ns2))
(defpackage :com.google.catharsis.xml.ns3 (:nicknames :ns3))

;;; Test case. See test emit-xml-node-complex for the corresponding XML.
(define-constant +xml-test+
  '(ns1::top-level-element (:@attr-1 "value") (ns1::@attr-2 "value") ns3::ns-decl
    (ns1::child (xml:text "fubar") (ns1::grand-child) (xml:text "baz>"))
    (ns2::child "text" "more text")
    (ns3::child (xml:cdata "<>"))
    (ns2::default-ns ns2::ns-decl-default
	(ns2::fubar (xml:lang "pl")
	 (:baz)))
    (:child)))


;;; Parsed data model.
(defclass xml-node () ())

(defclass xml-tag (xml-node)
  ((package-symbol  :initform nil  :initarg :package-symbol  :reader package-symbol)
   (tag-symbol      :initform nil  :initarg :tag-symbol      :reader tag-symbol)))

(defclass xml-element (xml-tag)
  ((child-list  :initform nil  :initarg :child-list  :reader child-list)))

(defclass xml-attribute (xml-tag)
  ((text-value  :initform nil  :initarg :text-value  :reader text-value)))

(defclass xml-ns-decl (xml-node)
  ((package-symbol  :initform nil  :initarg :package-symbol  :reader package-symbol)
   (default-p       :initform nil  :initarg :default-p       :reader default-p)))

(defclass xml-text (xml-node)
  ((text-value  :initform nil  :initarg :text-value  :reader text-value)))

(defclass xml-cdata (xml-text) ())


;;; Parser for the S-expression representation of XML.
; TODO: support language embedding (e.g. JavaScript and CSS within HTML) through generic parse-xml-tag parent-element tag

(defun parse-xml-node (node)
  (cond ((stringp node) (make-instance 'xml-text :text-value node))
	((numberp node) (make-instance 'xml-text :text-value (format nil "~A" node)))
	((atom node)
	 (let* ((symbol-name (symbol-name node))
		(ns-decl-p (string= symbol-name "NS-DECL"))
		(ns-decl-default-p (string= symbol-name "NS-DECL-DEFAULT"))
		(package-symbol (make-keyword (package-name (symbol-package node)))))
	   (unless (or ns-decl-p ns-decl-default-p) (error "Invalid node ~A." node))
	   (make-instance 'xml-ns-decl :package-symbol package-symbol :default-p ns-decl-default-p)))
	(t
	 (let* ((label (first node))
		(text-p (eq label 'xml:text))
		(cdata-p (eq label 'xml:cdata))
		(package-symbol (make-keyword (package-name (symbol-package label))))
		(tag-name (symbol-name label))
		(attr-p (char= (char tag-name 0) #\@))
		(xml-attr-p (eq package-symbol :com.google.catharsis.xml)))
	   (when (and attr-p (= (length tag-name) 1)) (error "Invalid attribute name."))
	   (cond ((or text-p cdata-p) (make-instance (if text-p 'xml-text 'xml-cdata)
						     :text-value (apply #'concatenate-strings-with-spaces
									(rest node))))
		 (attr-p (make-instance 'xml-attribute
					:package-symbol package-symbol
					:tag-symbol (make-keyword (subseq tag-name 1))
					:text-value (apply #'concatenate-strings-with-spaces (rest node))))
		 (xml-attr-p (make-instance 'xml-attribute
					    :package-symbol package-symbol
					    :tag-symbol (make-keyword tag-name)
					    :text-value
					    (apply #'concatenate-strings-with-spaces (rest node))))
		 (t (make-instance 'xml-element
				   :package-symbol package-symbol
				   :tag-symbol (make-keyword tag-name)
				   :child-list (mapcar #'parse-xml-node (rest node)))))))))

(test "parse-xml-node-simple"
      '((xml-text (text-value "simple-text"))
	(xml-text (text-value "text1 text2"))
	(xml-cdata (text-value "cdata1 cdata2"))
	(xml-ns-decl (default-p nil) (package-symbol :com.google.catharsis.xml.ns1))
	(xml-ns-decl (default-p t) (package-symbol :com.google.catharsis.xml.ns2))
	(xml-element (child-list nil) (package-symbol :com.google.catharsis.xml.ns1) (tag-symbol :tag))
	(xml-element (child-list nil) (package-symbol :keyword) (tag-symbol :tag))
	(xml-attribute (package-symbol :com.google.catharsis.xml.ns1) (tag-symbol :tag) (text-value ""))
	(xml-attribute (package-symbol :keyword) (tag-symbol :tag) (text-value "value1 value2")))
      (mapcar (compose #'object->sexp #'parse-xml-node)
	      '("simple-text"
		(xml:text "text1" "text2")
		(xml:cdata "cdata1" "cdata2")
		ns1::ns-decl
		ns2::ns-decl-default
		(ns1::tag)
		(:tag)
		(ns1::@tag)
		(:@tag "value1" "value2"))))

(test "parse-xml-node-complex"
      '(xml-element
	(child-list
	 ((xml-attribute (package-symbol :keyword) (tag-symbol :attr-1)
			 (text-value "value"))
	  (xml-attribute (package-symbol :com.google.catharsis.xml.ns1)
			 (tag-symbol :attr-2) (text-value "value"))
	  (xml-ns-decl (default-p nil)
		       (package-symbol :com.google.catharsis.xml.ns3))
	  (xml-element
	   (child-list
	    ((xml-text (text-value "fubar"))
	     (xml-element (child-list nil)
			  (package-symbol :com.google.catharsis.xml.ns1)
			  (tag-symbol :grand-child))
	     (xml-text (text-value "baz>"))))
	   (package-symbol :com.google.catharsis.xml.ns1)
	   (tag-symbol :child))
	  (xml-element
	   (child-list
	    ((xml-text (text-value "text"))
	     (xml-text (text-value "more text"))))
	   (package-symbol :com.google.catharsis.xml.ns2)
	   (tag-symbol :child))
	  (xml-element (child-list ((xml-cdata (text-value "<>"))))
		       (package-symbol :com.google.catharsis.xml.ns3)
		       (tag-symbol :child))
	  (xml-element
	   (child-list
	    ((xml-ns-decl (default-p t)
			  (package-symbol :com.google.catharsis.xml.ns2))
	     (xml-element
	      (child-list
	       ((xml-element (child-list ((xml-text (text-value "pl"))))
			     (package-symbol :com.google.catharsis.xml)
			     (tag-symbol :lang))
		(xml-element (child-list nil) (package-symbol :keyword)
			     (tag-symbol :baz))))
	      (package-symbol :com.google.catharsis.xml.ns2)
	      (tag-symbol :fubar))))
	   (package-symbol :com.google.catharsis.xml.ns2)
	   (tag-symbol :default-ns))
	  (xml-element (child-list nil) (package-symbol :keyword)
		       (tag-symbol :child))))
	(package-symbol :com.google.catharsis.xml.ns1)
	(tag-symbol :top-level-element))
      (object->sexp (parse-xml-node +xml-test+)))


;;; Generic functions that provide extensibility.
(defgeneric xml-ns-alias (package-symbol))
(defgeneric xml-ns-uri (package-symbol))
(defgeneric xml-tag-case (package-symbol tag-symbol))


(defmethod xml-ns-alias (package-symbol)
  "By default, the ns alias is the last component of the package name (downcased)."
  (let ((package-name (string-downcase (package-name (find-package package-symbol)))))
    (subseq package-name (1+ (or (position #\. package-name :from-end t :test #'char=) -1)))))

(defmethod xml-ns-alias ((package-symbol (eql :keyword)))
  "Keyword tags are namespace-less."
  nil)

(defmethod xml-ns-alias ((package-symbol (eql :com.google.catharsis.xml.ns1))) "ns1Alias")

(test "xml-ns-alias"
      '(nil "xml" "ns1Alias" "ns2")
      (mapcar #'xml-ns-alias '(:keyword :xml :com.google.catharsis.xml.ns1 :com.google.catharsis.xml.ns2)))


(defmethod xml-ns-uri ((package-symbol (eql :keyword)))
  "Keyword tags are namespace-less."
  nil)

(defmethod xml-ns-uri ((package-symbol (eql :com.google.catharsis.xml)))
  "The xml namespace is special--it is not declared."
  nil)

(defmethod xml-ns-uri ((package-symbol (eql :com.google.catharsis.xml.ns1))) "http://test.com/ns1")
(defmethod xml-ns-uri ((package-symbol (eql :com.google.catharsis.xml.ns2))) "http://test.com/ns2")
(defmethod xml-ns-uri ((package-symbol (eql :com.google.catharsis.xml.ns3))) "http://test.com/ns3")


(defmethod xml-tag-case (package-symbol tag-symbol)
  "By default, all tags are camel case."
  (declare (ignore package-symbol tag-symbol))
  :camel-case)


(defun additional-ns-decls (tag decls)
  "Computes required ns decls. Assumes that all local ns-decls from this tag have been added to decls."
  (let* ((alias (xml-ns-alias (package-symbol tag)))
	 (uri (xml-ns-uri (package-symbol tag)))
	 (cur-decl (assoc alias decls :test #'equal))
	 (cur-default-decl (assoc nil decls))
	 (default-ns-p (string= (cdr cur-default-decl) uri))
	 (decl-needed-p (and (string/= (cdr cur-decl) uri)
			     (not (and (typep tag 'xml-attribute)
				       (eq (package-symbol tag) :keyword)))
			     (not default-ns-p))))
    (nlet rec ((parsed-decls (when decl-needed-p
			       (list (make-instance 'xml-ns-decl
						    :package-symbol (package-symbol tag)
						    :default-p (eq (package-symbol tag) :keyword)))))
	       (assoc-decls (if decl-needed-p
				(cons (cons alias uri) decls)
				decls))
	       (attrs (remove-if-not (rcurry #'typep 'xml-attribute)
				     (and (typep tag 'xml-element)
					  (child-list tag)))))
      (if (null attrs)
	  (values parsed-decls assoc-decls)
	  (multiple-value-bind (parsed-decls-attr assoc-decls-attr)
	      (additional-ns-decls (first attrs) assoc-decls)
	    (rec (append parsed-decls-attr parsed-decls)
		 assoc-decls-attr
		 (rest attrs)))))))

(test "additional-ns-decls"
      '((nil nil)
	(nil ((nil . "http://test.com/ns1")))
	(nil (("ns1alias" . "http://test.com/ns1")))
	(((xml-ns-decl (default-p nil) (package-symbol :com.google.catharsis.xml.ns1)))
	 (("ns1alias" . "http://test.com/ns1")
	  ("ns1alias" . "http://test.com/ns1-not")))
	(((xml-ns-decl (default-p nil) (package-symbol :com.google.catharsis.xml.ns1)))
	 (("ns1alias" . "http://test.com/ns1")))
	(((xml-ns-decl (default-p nil) (package-symbol :com.google.catharsis.xml.ns1)))
	 (("ns1alias" . "http://test.com/ns1")))
	(((xml-ns-decl (default-p nil) (package-symbol :com.google.catharsis.xml.ns2))
	  (xml-ns-decl (default-p nil) (package-symbol :com.google.catharsis.xml.ns1)))
	 (("ns2" . "http://test.com/ns2")
	  ("ns1alias" . "http://test.com/ns1")))
	(((xml-ns-decl (default-p t) (package-symbol :keyword)))
	 ((nil) (nil . "http://test.com/ns1"))))
      (mapcar #'(lambda (input)
		  (multiple-value-bind (additional-ns-decls ns-decl-list)
		      (additional-ns-decls (parse-xml-node (first input))
					   (second input))
		    (list (mapcar #'object->sexp additional-ns-decls)
			  ns-decl-list)))
	      '(((:tag) nil)
		((ns1::child) ((nil . "http://test.com/ns1")))
		((ns1::child) (("ns1Alias" . "http://test.com/ns1")))
		((ns1::child) (("ns1Alias" . "http://test.com/ns1-NOT")))
		((ns1::child) nil)
		((ns1::child (ns1::@attr)) nil)
		((ns1::child (ns1::@attr) (ns2::@attr)) nil)
		((:tag) ((nil . "http://test.com/ns1"))))))


(defvar *ns-decl-list* nil
  "Assoc list: (alias . uri) in reverse stack order. nil alias used for default xmlns.")


(defun xml-tag-string (tag)
  "Computes the string that goes inside of the angle brackets."
  (let* ((ns-alias (xml-ns-alias (package-symbol tag)))
	 (ns-uri (xml-ns-uri (package-symbol tag)))
	 (default-ns-uri (cdr (assoc nil *ns-decl-list*)))
	 (adjusted-ns-alias (when (string/= ns-uri default-ns-uri) ns-alias)))
    (concatenate 'string
		 adjusted-ns-alias
		 (when adjusted-ns-alias ":")
		 (funcall (ecase (xml-tag-case (package-symbol tag) (tag-symbol tag))
			    (:lisp-case (compose #'string-downcase #'symbol-name))
			    (:pascal-case #'symbol->pascal-string)
			    (:camel-case #'symbol->camel-string)
			    (:upper-case #'symbol->upper-case-string)
			    (:lower-case #'symbol->lower-case-string))
			  (tag-symbol tag)))))

(test "xml-tag-string"
      '("ns1Alias:tag" "tag" "ns1Alias:tag" "tag" "ns2:tag")
      (mapcar (compose #'xml-tag-string #'parse-xml-node)
	      '((ns1::tag) (:tag) (ns1::@tag) (:@tag) (ns2::tag))))

(test "xml-tag-string-with-default-ns"
      "tag"
      (let ((*ns-decl-list* '((nil . "http://test.com/ns1"))))
	(xml-tag-string (parse-xml-node '(ns1::tag)))))


(defun xml-escape (str)
  (apply #'concatenate
	 'string
	 (reduce #'(lambda (char escaped-str)
		     (cons (case char
			     (#\" "&quot;")
			     (#\' "&apos;")
			     (#\& "&amp;")
			     (#\< "&lt;")
			     (#\> "&gt;")
			     (#\\ "\\")
			     (t (string char)))
			   escaped-str))
		 str
		 :from-end t
		 :initial-value nil)))

(test "xml-escape"
      "123-&quot;-&apos;-&amp;-&lt;-&gt;-\\-"
      (xml-escape "123-\"-'-&-<->-\\-"))


(defgeneric emit-xml-node (node gen))

(defmethod emit-xml-node ((node xml-element) gen)
  (let* ((attrs (remove-if-not (rcurry #'typep 'xml-attribute) (child-list node)))
	 (ns-decls (remove-if-not (rcurry #'typep 'xml-ns-decl)
				  (child-list node)))
	 (child-node-list (remove-if-not #'(lambda (node)
					     (or (typep node 'xml-element)
						 (typep node 'xml-text)))
					 (child-list node)))
	 (multiple-lines (some #'(lambda (node)
				   (or (typep node 'xml-element)
				       (typep node 'xml-cdata)))
			       child-node-list)))
    (multiple-value-bind (additional-ns-decls *ns-decl-list*)
	(additional-ns-decls node
			     (append (mapcar #'(lambda (ns-decl)
						 (cons (unless (default-p ns-decl)
							 (xml-ns-alias (package-symbol ns-decl)))
						       (xml-ns-uri (package-symbol ns-decl))))
					     ns-decls)
				     *ns-decl-list*))
      (emit-list gen
		 '("<" t nil)
		 (list (xml-tag-string node) nil t nil)
		 'push-wrap-marker)
      (mapc (rcurry #'emit-xml-node gen) attrs)
      (mapc (rcurry #'emit-xml-node gen) additional-ns-decls)
      (mapc (rcurry #'emit-xml-node gen) ns-decls)
      (cond (multiple-lines (emit-list gen
				       '(">" nil t nil)
				       'pop-wrap-marker
				       'new-line
				       'increase-indent)
			    (mapc (rcurry #'emit-xml-node gen) child-node-list)
			    (emit-list gen
				       'decrease-indent
				       'new-line
				       '("</" t nil)
				       (list (xml-tag-string node) nil nil nil)
				       '(">" nil t nil)
				       'new-line))
	    (child-node-list (emit-list gen
					'(">" nil nil nil)
					'push-wrap-marker)
			     (mapc (rcurry #'emit-xml-node gen) child-node-list)
			     (emit-list gen
					'pop-wrap-marker
					'pop-wrap-marker
					'("</" nil nil)
					(list (xml-tag-string node) nil nil nil)
					'(">" nil t nil)
					'new-line))
	    (t (emit-list gen
			  'pop-wrap-marker
			  '("/>" nil t nil)
			  'new-line))))))


(defmethod emit-xml-node ((node xml-attribute) gen)
  (emit-list gen
	     (list (xml-tag-string node) t nil)
	     '("=\"" nil nil nil)
	     (list (xml-escape (text-value node)) nil nil nil)
	     '("\"" nil t nil)))

(defmethod emit-xml-node ((node xml-ns-decl) gen)
  (if (default-p node)
      (emit-list gen
		 '("xmlns=\"" t nil)
		 (list (or (xml-ns-uri (package-symbol node)) "")
		       nil nil nil)
		 '("\"" nil t nil))
      (emit-list gen
		 '("xmlns:" t nil)
		 (list (xml-ns-alias (package-symbol node)) nil nil nil)
		 '("=\"" nil nil nil)
		 (list (xml-ns-uri (package-symbol node)) nil nil nil)
		 '("\"" nil t nil))))

(defmethod emit-xml-node ((node xml-text) gen)
  (emit-token gen (xml-escape (text-value node))))

(defmethod emit-xml-node ((node xml-cdata) gen)
  (emit-list gen
	     '("<![CDATA[" t nil t)
	     (list (text-value node) nil nil nil)
	     '("]]>" nil t nil)))


(defun make-xml-generator (stream)
  (make-instance 'source-file-generator :output-file stream))


(test "emit-xml-node-simple"
      '("simple-text"
	"text1 text2"
	"<![CDATA[cdata1 cdata2]]>"
	"xmlns:ns1Alias=\"http://test.com/ns1\""
	"xmlns=\"http://test.com/ns2\""
	"<ns1Alias:tag xmlns:ns1Alias=\"http://test.com/ns1\"/>"
	"<tag/>"
	"ns1Alias:tag=\"\""
	"tag=\"value1 value2\"")
      (mapcar #'(lambda (xml-node)
		  (with-output-to-string (out)
		    (let ((gen (make-xml-generator out)))
		      (emit-xml-node (parse-xml-node xml-node) gen))))
	      '("simple-text"
		(xml:text "text1" "text2")
		(xml:cdata "cdata1" "cdata2")
		ns1::ns-decl
		ns2::ns-decl-default
		(ns1::tag)
		(:tag)
		(ns1::@tag)
		(:@tag "value1" "value2"))))


(test "emit-xml-node-complex"
  "
<ns1Alias:topLevelElement attr1=\"value\" ns1Alias:attr2=\"value\" xmlns:ns1Alias=\"http://test.com/ns1\"
                          xmlns:ns3=\"http://test.com/ns3\">
  <ns1Alias:child>
    fubar <ns1Alias:grandChild/>
    baz&gt;
  </ns1Alias:child>
  <ns2:child xmlns:ns2=\"http://test.com/ns2\">text more text</ns2:child>
  <ns3:child>
    <![CDATA[<>]]>
  </ns3:child>
  <defaultNs xmlns=\"http://test.com/ns2\">
    <fubar xml:lang=\"pl\">
      <baz xmlns=\"\"/>
    </fubar>
  </defaultNs>
  <child/>
</ns1Alias:topLevelElement>"
  (with-output-to-string (out)
    (let ((gen (make-xml-generator out)))
      (emit-new-line gen)
      (emit-xml-node (parse-xml-node +xml-test+) gen))))
