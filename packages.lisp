(in-package :cl-user)

(defpackage :com.google.catharsis
  (:nicknames :catharsis)
  (:use :com.google.common
        :split-sequence
        :cl)
  (:shadowing-import-from :com.google.common :comma)
  (:export :?  :compile-language  :parse-program  :generate-sexp  :post-process
           :lookup-def  :parent  :id  :env  :read-zexp-file
           :make-protobuf-text-generator  :emit-protobuf-text
           :make-xml-generator  :parse-xml-node  :emit-xml-node  :xml-ns-alias  :xml-ns-uri  :xml-tag-case
           :def-internal-type  :def-zexp-type  :def-zexp-form  :def-sexp-form  :def-ir-class  :def-prop
           :internal-type  :zexp-type  :zexp-form  :sexp-form  :syntax  :end-of-list  :ir-class  :ir-overlay
           :local-env  :inherit  :flag  :type  :parse-by-type  :repeating  :optional
           :default-value  :slot-name  :accessor))

(defpackage :com.google.catharsis.code-gen.upper-case
  (:nicknames :upper-case))

(defpackage :com.google.catharsis.code-gen.lower-case
  (:nicknames :lower-case))

(defpackage :com.google.catharsis.code-gen.lower-case_
  (:nicknames :lower-case_))

(defpackage :com.google.catharsis.code-gen.pascal-case
  (:nicknames :pascal-case))

(defpackage :com.google.catharsis.code-gen.camel-case
  (:nicknames :camel-case))

(dolist (package (mapcar #'find-package '(:upper-case :lower-case :lower-case_ :pascal-case :camel-case)))
  (do-symbols (sym (find-package :cl) nil)
    (shadow sym package)))

;;; N.B.: Commented out symbols come from com.google.catharsis due to a naming conflict.
(defpackage :com.google.catharsis.cpp
  (:nicknames :cpp)
  (:use :com.google.catharsis
        :com.google.common
        :cl)
  (:export ::_  ://  :define  :ifndef  :std-include  :include  :define  :macro-call  :enum
           :namespace  :using  :class-forward  :friend
           :virtual  :abstract  :explicit  :public  :protected  :private  :static  :const  :volatile  :extern  :inherit
           :struct  :field  :class  :inherit  :method  :constructor  :destructor  :init  :constructor-call
           :type  :return-type  :template  :ptr  :ref  :ns-ref  :param
           :char-literal  :long-char-literal  :string-literal  :long-string-literal  :class-ref  :field-ref-type
           #|:group|#
           :call  :call-method  :call-static  :call-ptr  :call-this  :call-template
           :static-cast  :dynamic-cast  :const-cast  :reinterpret-cast
           :post++  :post--
           :new  :delete  :call  :array-ref  :array-size  :field-ref  :field-ref-ptr  :field-ref-this  :field-ref-static
           :++  :--  :positive  :negative  :~  :!  :addr  :deref  :new  :sizeof  :cast
           :member-ref  :member-ref-ptr
           :*  :/  :%
           :+  :-
           :<<  :>>  :>>>
           :<  :>  :<=  :>=  :instanceof
           :==  :!=
           :bit-and  :bit-xor  :bit-or
           :and  :or  :?
           :set  :=  :+=  :-=  :*=  :/=  :%=  :bit-and=  :bit-xor=  :bit-or=  :<<=  :>>=  :>>>=
           :progn
           :null-body
           :var  :while
           :return  :goto  :throw  :break  :continue
           :block  :if  :for  :do  :switch  :try  :label
           :case  :default
           :catch  :finally
           :cond)
  (:shadowing-import-from :com.google.catharsis :?))

(defpackage :com.google.catharsis.java
  (:nicknames :java)
  (:use	:com.google.catharsis
        :com.google.common
        :cl)
  (:export :generate
           :java
           :package
           ://  :javadoc  :@  :arg
           :import
           :class  :interface  :enum
           :constant
           :public  :protected  :private
           :static  :abstract  :final  :native  :synchronized  :transient  :volatile
           :extends  :implements
           :constructor  :method
           :return-type  :param  :throws
           :field  :static-initializer  :init  :type  :array
           :template  :template-def
           :char-literal  :string-literal  :array-literal  :class-ref
           #|:group|#
           :array-ref  :new  :call  :call-this  :call-super  :field-ref  :field-ref-this  :field-ref-super  :class-object
           :post++  :post--
           :++  :--  :positive  :negative  :~  :!  :cast
           :*  :/  :%
           :+  :-
           :<<  :>>  :>>>
           :<  :>  :<=  :>=  :instanceof
           :==  :!=
           :bit-and  :bit-xor  :bit-or
           :and  :or  :?
           :set  :=  :+=  :-=  :*=  :/=  :%=  :bit-and=  :bit-xor=  :bit-or=  :<<=  :>>=  :>>>=
           :progn
           :_  :var  :while
           :return  :goto  :throw  :break  :continue  :assert
           :block  :synchronized-block  :if  :for  :for-each  :do  :switch  :try  :label
           :this-constructor  :super-constructor
           :case  :default
           :catch  :finally
           :cond  :prop
           :doc)
  (:shadowing-import-from :com.google.catharsis :?))

(defpackage :com.google.catharsis.javascript
  (:nicknames :javascript :js)
  (:use	:com.google.catharsis
        :com.google.common
        :cl)
  (:export :generate
           :module
           ://
           :class  :prototype  :container  :constructor  :method
           :function  :constructor  :lambda  :params  :thunk-call
           :string-literal  :array-literal
           :object-literal  :prop
           :regex-literal
           #|:group|#
           :field-ref  :field-ref-this  :field-ref-self
           :funcall  :apply  :prototype-ref  :array-ref  :new  :call  :call-this  :call-self
           :post++  :post--  :++  :--  :positive  :negative  :~  :!  #|:delete|#  :typeof  :void
           :*  :/  :%
           :+  :-
           :<<  :>>  :>>>
           :<  :>  :<=  :>=  :instanceof  :in
           :==  :!=  :===  :!==
           :bit-and  :bit-xor  :bit-or
           :and  :or  :?
           :set  :=  :+=  :-=  :*=  :/=  :%=  :bit-and=  :bit-xor=  :bit-or=  :<<=  :>>=  :>>>=
           :progn
           :type-ref
           :_  :var  :while
           :return  :goto  :throw  :break  :continue
           :block  :with  :if  :for  :for-each  :do  :switch  :try  :label
           :case  :default
           :catch  :finally
           :cond)
  (:shadowing-import-from :com.google.catharsis :?))

(defpackage :com.google.catharsis.proto-buf
  (:nicknames :proto-buf :pb)
  (:use	:com.google.catharsis
        :com.google.common
        :cl)
  (:export :package :doc :author :email
           :c++header :java :python :sawzall
           :message :field :type :default :weak
           #|:group|# :enum :value
                      :id :obsolete
                      :repeated :required :optional
                      :service :rpc :param :streams :returns :option))
