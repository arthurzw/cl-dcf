(in-package :com.google.catharsis)

(defun make-protobuf-text-generator (stream)
  (make-instance 'source-file-generator :output-file stream))


(defun protobuf-escape (str)
  (apply #'concatenate
         'string
         (reduce #'(lambda (char escaped-str)
                     (cons (case char
                             (#\" "\\\"")
                             (t (string char)))
                           escaped-str))
                 str
                 :from-end t
                 :initial-value nil)))


(defun emit-protobuf-attr (attr gen)
  (assert (= (length attr) 2))
  (let ((attr-value (second attr)))
    (emit-list gen
               'lower-case (first attr)
               '(": " nil nil nil)
               (if (stringp attr-value)
                   (concatenate 'string "\"" (protobuf-escape attr-value) "\"")
                   attr-value)
               'new-line)))

(test "emit-protobuf-attr"
      "int_attr: 76
float_attr: 19.76
string_attr: \"string with \\\"quotes\\\"\"
enum_attr: kEnumValue
camelCaseAttr: 1"
      (with-output-to-string (out)
        (let ((gen (make-protobuf-text-generator out)))
          (emit-protobuf-attr '(:int-attr 76) gen)
          (emit-protobuf-attr '(:float-attr 19.76) gen)
          (emit-protobuf-attr '(:string-attr "string with \"quotes\"") gen)
          (emit-protobuf-attr '(:enum-attr :k-enum-value) gen)
          (emit-protobuf-attr '(camel-case::camel-case-attr 1) gen))))


(defun emit-protobuf-node (node gen)
  (when node
    (assert (atom (first node)))
    (let ((attr-p (and (= (length node) 2)
                       (atom (second node)))))
      (if attr-p
          (emit-protobuf-attr node gen)
          (progn
            (emit-list gen
                       'lower-case (first node)
                       '("{" t t nil)
                       'increase-indent
                       'new-line)
            (dolist (child (rest node))
              (emit-protobuf-node child gen))
            (emit-list gen
                       'decrease-indent
                       'new-line
                       '("}" t t nil)
                       'new-line))))))

(defun emit-protobuf-text (node-list gen)
  (dolist (node node-list)
    (emit-protobuf-node node gen)))


(test "emit-protobuf-text"
      "root_node {
  int_attr: 76
  float_attr: 19.76
  string_attr: \"A\"
  enum_attr: kEnumValue
  PascalCaseNode {
    nested_attr: 1
    nested_attr: 2
  }
}
second_node {
  foo: \"bar\"
}"
      (with-output-to-string (out)
        (let ((gen (make-protobuf-text-generator out)))
          (emit-protobuf-text '((:root-node
                                 (:int-attr 76)
                                 (:float-attr 19.76)
                                 nil
                                 (:string-attr "A")
                                 (:enum-attr :k-enum-value)
                                 (pascal-case::pascal-case-node
                                  (:nested-attr 1)
                                  (:nested-attr 2)))
                                (:second-node (:foo "bar")))
                              gen))))
