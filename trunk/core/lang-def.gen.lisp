(in-package :com.google.catharsis)

;;; Meta-language definition.

(defclass internal-type-definition (definition)
  ((prop-tags :accessor prop-tags :initarg :prop-tags :initform nil :allocation :class)
   (class-name :accessor class-name :initform nil)
   (inherit-list :accessor inherit-list :initarg :inherit-list :initform nil)
   (prop-list :accessor prop-list :initarg :prop-list :initform nil)
   (ir-class-name :accessor ir-class-name :initform nil)))

(setf (prop-tags (make-instance 'internal-type-definition))
      '(class-name inherit))

(defclass zexp-type-definition (internal-type-definition)
  ((prop-tags :accessor prop-tags :initarg :prop-tags :initform nil :allocation :class)))

(setf (prop-tags (make-instance 'zexp-type-definition))
      '(class-name inherit))

(defclass zexp-form-definition (zexp-type-definition)
  ((prop-tags :accessor prop-tags :initarg :prop-tags :initform nil :allocation :class)
   (local-env-p :accessor local-env-p :initform nil)))

(setf (prop-tags (make-instance 'zexp-form-definition))
      '(class-name inherit local-env))

(defclass zexp-prop-definition (definition)
  ((prop-tags :accessor prop-tags :initarg :prop-tags :initform nil :allocation :class)
   (data-type :accessor data-type :initform nil)
   (flag-p :accessor flag-p :initform nil)
   (default-value :accessor default-value :initform nil)
   (repeating-p :accessor repeating-p :initarg :repeating-p :initform nil)
   (parse-by-type-p :accessor parse-by-type-p :initform nil)
   (slot-name :accessor slot-name :initform nil)
   (accessor :accessor accessor :initform nil)
   (class-allocation-p :accessor class-allocation-p :initform nil)
   (ir-slot-name :accessor ir-slot-name :initform nil)
   (ir-accessor-name :accessor ir-accessor-name :initform nil)
   (ir-lisp-data-type :accessor ir-lisp-data-type :initform nil)
   (ir-user-data-type :accessor ir-user-data-type :initform nil)))

(setf (prop-tags (make-instance 'zexp-prop-definition))
      '(type flag default-value repeating parse-by-type slot-name accessor class-allocation))

(defclass sexp-form-definition (internal-type-definition)
  ((prop-tags :accessor prop-tags :initarg :prop-tags :initform nil :allocation :class)
   (syntax :accessor syntax :initarg :syntax :initform nil)))

(setf (prop-tags (make-instance 'sexp-form-definition))
      '(inherit syntax))

(defclass ir-overlay-definition (definition)
  ((prop-tags :accessor prop-tags :initarg :prop-tags :initform nil :allocation :class)
   (ir-class-list :accessor ir-class-list :initform nil)))

(defclass ir-class-definition (internal-type-definition)
  ((prop-tags :accessor prop-tags :initarg :prop-tags :initform nil :allocation :class)))

(setf (prop-tags (make-instance 'ir-class-definition))
      '(class-name inherit))

;;; Meta-language parser.

(define-constant +dls-language+
  (make-instance
   'language
   :dispatch-table (bulk-insert-trie '(((internal-type) internal-type-definition)
				       ((zexp-type) zexp-type-definition)
				       ((zexp-form) zexp-form-definition)
				       ((prop) zexp-prop-definition)
				       ((sexp-form) sexp-form-definition)
				       ((ir-overlay) ir-overlay-definition)
				       ((ir-class) ir-class-definition))
				     (make-instance 'trie))
   :macro-dispatch-table (make-hash-table)))

;; internal-type-definition

(defmethod initialize-instance ((obj internal-type-definition) &rest initargs &key)
  (declare (ignore initargs))
  (call-next-method)
  (setf (local-env obj) (make-local-env :id (id obj))))

(defmethod parse-zexp-property-by-tag ((obj internal-type-definition) (tag (eql 'class-name)) body)
  (setf (class-name obj) (parse-zexp-prop-value-phase-2 obj (first body))))

(defmethod parse-zexp-property-by-tag ((obj internal-type-definition) (tag (eql 'inherit)) body)
  (append1f (inherit-list obj) (parse-zexp-prop-value-phase-2 obj (first body))))

(defmethod parse-zexp-property-by-type ((obj internal-type-definition) (def zexp-prop-definition))
  (append1f (prop-list obj) def))

;; zexp-form-definition

(defmethod parse-zexp-property-by-tag ((obj zexp-form-definition) (tag (eql 'local-env)) body)
  (declare (ignore body))
  (setf (local-env-p obj) t))

;; zexp-prop-definition

(defmethod parse-zexp-property-by-tag ((obj zexp-prop-definition) (tag (eql 'type)) body)
  (setf (data-type obj) (parse-zexp-prop-value-phase-2 obj (first body))))

(defmethod parse-zexp-property-by-tag ((obj zexp-prop-definition) (tag (eql 'flag)) body)
  (declare (ignore body))
  (setf (flag-p obj) t))

(defmethod parse-zexp-property-by-tag ((obj zexp-prop-definition) (tag (eql 'default-value)) body)
  (setf (default-value obj) (parse-zexp-prop-value-phase-2 obj (first body))))

(defmethod parse-zexp-property-by-tag ((obj zexp-prop-definition) (tag (eql 'repeating)) body)
  (declare (ignore body))
  (setf (repeating-p obj) t))

(defmethod parse-zexp-property-by-tag ((obj zexp-prop-definition) (tag (eql 'parse-by-type)) body)
  (declare (ignore body))
  (setf (parse-by-type-p obj) t))

(defmethod parse-zexp-property-by-tag ((obj zexp-prop-definition) (tag (eql 'slot-name)) body)
  (setf (slot-name obj) (parse-zexp-prop-value-phase-2 obj (first body))))

(defmethod parse-zexp-property-by-tag ((obj zexp-prop-definition) (tag (eql 'accessor)) body)
  (setf (accessor obj) (parse-zexp-prop-value-phase-2 obj (first body))))

(defmethod parse-zexp-property-by-tag ((obj zexp-prop-definition) (tag (eql 'class-allocation)) body)
  (declare (ignore body))
  (setf (class-allocation-p obj) t))

;; sexp-form-definition

(defmethod parse-zexp-property-by-tag ((obj sexp-form-definition) (tag (eql 'syntax)) body)
  (setf (syntax obj) (parse-zexp-prop-value-phase-2 obj (first body))))

;; ir-overlay-definition

(defmethod initialize-instance ((obj ir-overlay-definition) &rest initargs &key)
  (declare (ignore initargs))
  (call-next-method)
  (setf (local-env obj) (make-local-env :id (id obj))))

(defmethod parse-zexp-property-by-type ((obj ir-overlay-definition) (def ir-class-definition))
  (append1f (ir-class-list obj) def))

;;; sexp-language-definition

(PROGN
 (DEFINE-CONSTANT +SEXP-PARSER-LANGUAGE+
   (MAKE-INSTANCE 'LANGUAGE
                  :DISPATCH-TABLE
                  (BULK-INSERT-TRIE
                   '(((SEXP-SYNTAX-CONST) SEXP-SYNTAX-CONST)
                     ((SEXP-SYNTAX-PROP) SEXP-SYNTAX-PROP)
                     ((SEXP-SYNTAX-EXP) SEXP-SYNTAX-EXP))
                   (MAKE-INSTANCE 'TRIE))
                  :MACRO-DISPATCH-TABLE
                  (LET ((MDT (MAKE-HASH-TABLE)))
                    MDT)
                  :SEXP-PARSER-FN
                  'PARSE-SEXP-SEXP-PARSER))
 (DEFCLASS SEXP-SYNTAX
           (DEFINITION)
           ((PROP-TAGS :ACCESSOR PROP-TAGS :INITFORM NIL :ALLOCATION :CLASS)))
 (SETF (PROP-TAGS (MAKE-INSTANCE 'SEXP-SYNTAX)) 'NIL)
 (DEFCLASS SEXP-SYNTAX-CONST
           (SEXP-SYNTAX)
           ((PROP-TAGS :ACCESSOR PROP-TAGS :INITFORM NIL :ALLOCATION :CLASS)
            (VALUE :ACCESSOR
                   VALUE
                   :INITARG
                   :VALUE
                   :INITFORM
                   NIL
                   :ALLOCATION
                   :INSTANCE)))
 (SETF (PROP-TAGS (MAKE-INSTANCE 'SEXP-SYNTAX-CONST)) '(VALUE))
 (DEFCLASS SEXP-SYNTAX-PROP
           (SEXP-SYNTAX)
           ((PROP-TAGS :ACCESSOR PROP-TAGS :INITFORM NIL :ALLOCATION :CLASS)
            (NAME :ACCESSOR
                  NAME
                  :INITARG
                  :NAME
                  :INITFORM
                  NIL
                  :ALLOCATION
                  :INSTANCE)))
 (SETF (PROP-TAGS (MAKE-INSTANCE 'SEXP-SYNTAX-PROP)) '(NAME))
 (DEFCLASS SEXP-SYNTAX-EXP
           (SEXP-SYNTAX-PROP)
           ((PROP-TAGS :ACCESSOR PROP-TAGS :INITFORM NIL :ALLOCATION :CLASS)
            (TYPE-CLASS :ACCESSOR
                        TYPE-CLASS
                        :INITARG
                        :TYPE-CLASS
                        :INITFORM
                        NIL
                        :ALLOCATION
                        :INSTANCE)
            (BODY-LIST :ACCESSOR
                       BODY-LIST
                       :INITARG
                       :BODY-LIST
                       :INITFORM
                       NIL
                       :ALLOCATION
                       :INSTANCE)))
 (SETF (PROP-TAGS (MAKE-INSTANCE 'SEXP-SYNTAX-EXP)) '(NAME TYPE-CLASS))
 (DEFMETHOD PARSE-ZEXP-PROPERTY-BY-TAG
            ((OBJ SEXP-SYNTAX-CONST) (TAG (EQL 'VALUE)) BODY)
            (LET ((PROP-VALUE (PARSE-ZEXP-PROP-VALUE-PHASE-2 OBJ (FIRST BODY))))
              (IF (VALUE OBJ)
                  (ERROR "Duplicate instance of property ~A." 'VALUE)
                  (SETF (VALUE OBJ) PROP-VALUE))))
 (DEFMETHOD PARSE-ZEXP-PROPERTY-BY-TAG
            ((OBJ SEXP-SYNTAX-PROP) (TAG (EQL 'NAME)) BODY)
            (LET ((PROP-VALUE (PARSE-ZEXP-PROP-VALUE-PHASE-2 OBJ (FIRST BODY))))
              (IF (NAME OBJ)
                  (ERROR "Duplicate instance of property ~A." 'NAME)
                  (SETF (NAME OBJ) PROP-VALUE))))
 (DEFMETHOD PARSE-ZEXP-PROPERTY-BY-TAG
            ((OBJ SEXP-SYNTAX-EXP) (TAG (EQL 'TYPE-CLASS)) BODY)
            (LET ((PROP-VALUE (PARSE-ZEXP-PROP-VALUE-PHASE-2 OBJ (FIRST BODY))))
              (IF (TYPE-CLASS OBJ)
                  (ERROR "Duplicate instance of property ~A." 'TYPE-CLASS)
                  (SETF (TYPE-CLASS OBJ) PROP-VALUE))))
 (DEFMETHOD PARSE-ZEXP-PROPERTY-BY-TYPE
            ((OBJ SEXP-SYNTAX-EXP) (DEF SEXP-SYNTAX))
            (LET ((PROP-VALUE DEF))
              (UNLESS (TYPEP PROP-VALUE 'SEXP-SYNTAX)
                (ERROR "Invalid value of property ~A. ~A expected."
                       'BODY
                       'SEXP-SYNTAX))
              (APPEND1F (BODY-LIST OBJ) PROP-VALUE)))
 (DEFMETHOD INITIALIZE-INSTANCE
            ((OBJ SEXP-SYNTAX-EXP) &REST INITARGS &KEY)
            (DECLARE (IGNORE INITARGS))
            (CALL-NEXT-METHOD)
            (SETF (LOCAL-ENV OBJ) (MAKE-LOCAL-ENV :ID (ID OBJ)))))
