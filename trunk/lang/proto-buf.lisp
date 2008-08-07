(in-package :com.google.catharsis.proto-buf)

(def-grammar proto-buf

  (rule <package> ::=
	"package" 'lower-case :name 'semicolon 'new-line 'vertical-space
	(* <doc>)
	(* (one-of <c++header> <java> <python> <sawzall>))
	(* <message>)
	(* <service>))

  (rule <package> <doc> ::=
	"//" (* :comment-string) 'new-line
	(? "// Authors:" 'new-line
	   <author> (* <author>)))

  (rule <package> <doc> <author> ::=
	"//   " :author-name (? <email>) 'new-line)

  (rule <package> <doc> <author> <email> ::=
	'open-paren :email 'close-paren)

  (rule <package> <c++header> ::=
	'new-line
	'("c++header #include \"" t nil) :include '("\"" nil t))

  (rule <package> <java> ::=
	'new-line
	'("java import " t nil) :include 'semicolon)

  (rule <package> <python> ::=
	'new-line
	'("python from " t nil) :include "import *")

  (rule <package> <sawzall> ::=
	'new-line
	'("sawzall proto \"" t nil) :include '("\"" nil t))

  (rule <message> ::=
	'vertical-space
	"parsed message" 'pascal-case :name 'begin-block
	(* (one-of <field> <group> <enum>))
	'end-block-no-new-line 'semicolon 'new-line)

  (rule <field> ::=
	(? <doc>)
	(unordered (one-of <repeated> <required> <optional>)
		   <type>
		   (group (? <obsolete>) 'lower-case :name)
		   "=" (? <id>)
		   (? <default>)
		   (? <weak>))
	'semicolon 'new-line)

  (rule <field> <type> ::= (one-of :type <message>))

  (rule <field> <type> <message> ::=
	'("message<" t nil)
	(? <package>) 'pascal-case :message-type
	'(">" nil t))

  (rule <field> <type> <message> <package> ::=
	(* 'lower-case :package-component '("." nil nil)))

  (rule <field> <default> ::= "[default =" :value '("]" nil nil))

  (rule <field> <weak> ::= '("[weak = true]" t nil))

  (rule <group> ::=
	'vertical-space
	(? <doc>)
	(unordered (one-of <repeated> <required> <optional>)
		   "group"
		   (group (? <obsolete>) 'pascal-case :name)
		   "=" (? <id>))
	'begin-block
	(* (one-of <field> <group> <enum>))
	'end-block-no-new-line 'semicolon 'new-line 'vertical-space)

  (rule <enum> ::=
	'vertical-space
	(? <doc>)
	"enum" 'pascal-case :name 'begin-block
	(* <value>)
	'end-block-no-new-line 'semicolon 'new-line 'vertical-space)

  (rule <enum> <value> ::=
	'new-line
	'upper-case :name
	"=" :value)

  (rule <id> ::= :id)

  (rule <obsolete> ::= '("OBSOLETE_" t nil))

  (rule <field.group.enum> <doc> ::= "//" (* :comment-string) 'new-line)

  (nullary-rule <repeated>)
  (nullary-rule <required>)
  (nullary-rule <optional>)

  (rule <service> ::=
	'vertical-space
	"service" 'pascal-case :name 'begin-block
	(* <rpc>)
	'end-block-no-new-line 'semicolon 'new-line)

  (rule <service> <rpc> ::=
	"rpc" 'pascal-case :name 'open-paren-no-space <param> 'close-paren
	(? <streams>)
	(? <returns>)
	'begin-block
	(* <option>)
	'end-block-no-new-line 'semicolon 'new-line)

  (rule <service> <rpc> <param> ::= 'pascal-case :request-type)
  (rule <service> <rpc> <streams> ::= "streams" 'open-paren-no-space 'pascal-case :stream-type 'close-paren)
  (rule <service> <rpc> <returns> ::= "returns" 'open-paren-no-space 'pascal-case :return-type 'close-paren)
  (rule <service> <rpc> <option> ::= 'new-line "option" 'lower-case :name "=" :value 'semicolon))


(defun make-proto-buf-generator (&optional (file-name *standard-output*))
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


(defconstant +proto-buf-test+
  '(package :bigtable
    (doc "Definitions for client-level interface ..."
     (author "Jeff Dean" (email "jeff@google.com"))
     (author "Sanjay Ghemawat" (email "sanjay@google.com")))

    (c++header "statmessage.pb.h")
    (java "com.google.searchbench.StatMessage")
    (python "google3.searchbench.statmessage_pb")
    (sawzall "searchbench/statmessage.proto")

    (message :set-args
     (field :row (type :string) (required))
     (group :column (repeated)
      (field :name (type :string) (required))
      (field :value (type :string) (required))
      (field :replace (doc "If 'replace' is set...")
	     (type :bool) (optional) (id 10))
      (field :delete-older (type :bool) (optional))
      (field :micro-timestamp (type :int64) (optional) (id 9))
      (field :timestamp-xxx (type :int64) (optional) (obsolete)))
     (enum :bloomfilter-type
      (value :bloomfilter-all 0)
      (value :bloomfilter-nobase 1)
      (value :bloomfilter-none 2))
     (field :micro-timestamp (type :int64) (optional))
     (field :replicate (type :bool) (optional) (id 16) (default :true))
     (field :backup-policy (type (message (package :bigtable) :backup-policy)) (optional) (weak) (id 11)))

    (service :stock-service
     (rpc :watch-future-trades (param :stock-request) (streams :stock-reply) (returns :empty-message)
      (option :deadline 1.0)
      (option :duplicate-suppression :true)
      (option :protocol :udp)))))

;;; Test for hand execution.
(eval-when (:execute)
  (prog1 nil
    (generate-sexp (make-proto-buf-generator) #'proto-buf-parse +proto-buf-test+)))
