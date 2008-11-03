(in-package :com.google.common)


(defun maptree (fn tree)
  "Recursively applies function fn to all members of tree t."
  (mapcar #'(lambda (x)
              (if (listp x)
                  (maptree fn x)
                  (funcall fn x)))
          tree))

(export 'maptree)

(test "maptree"
      '(2 (4 (6 8) 10) () 12)
      (maptree #'(lambda (x) (* x 2)) '(1 (2 (3 4) 5) () 6)))


(defun findtree (item tree &key (test #'eql) key)
  "Finds the supplied item in the tree.
   The comparison is performed using the test function.
   If key is specified, then this function is applied before performing the comparison.
   The found item is returned."
  (reduce #'(lambda (found-item x)
              (if found-item
                  found-item
                  (if (listp x)
                      (findtree item x :test test :key key)
                      (when (funcall test
                                     (if key (funcall key x) x)
                                     item)
                        x))))
          (cons nil tree)))

(export 'findtree)

(test "findtree"
      'z
      (findtree 'z '(1 2 3 () (4 5) (6 z))))


(defmacro define-constant (name value &optional doc)
  "String ANSI implementations (like SBCL) will barf at naked
DEFCONSTANT forms when compiling and subsequently loading them (as in
slime's C-cC-k) if the corresponding values are not EQ (technically,
we're redefining the constant). This macro does the right thing."
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

(export 'define-constant)


(defun hashtable->list (ht)
  "Creates a list of (k v) pairs from the hashtable."
  (let ((result nil))
    (maphash #'(lambda (&rest k-v) (push k-v result)) ht)
    (nreverse result)))

(export 'hashtable->list)

(test "hashtable->list"
      '((1 nil) (2 (2)))
      (let ((ht (make-hash-table)))
        (setf (gethash 1 ht) nil)
        (setf (gethash 2 ht) '(2))
        (hashtable->list ht)))


(defun list->hashtable (list)
  "Creates a hashtable from a list of (k v) pairs."
  (let ((ht (make-hash-table)))
    (dolist (x list)
      (setf (gethash (first x) ht) (second x)))
    ht))

(export 'list->hashtable)

(test "list->hashtable"
      '((1 nil) (2 (2)))
      (hashtable->list (list->hashtable '((1 nil) (2 (2))))))


(defun add-separators (sep list)
  "Inserts an instance of sep in-between list members of list.
   For example, when applied to ('AND (1 2 3)), it returns (1 AND 2 AND 3)."
  (rest (mappend #'(lambda (x) (list sep x)) list)))

(export 'add-separators)

(test "add-separators"
      '(1 and 2 and 3)
      (add-separators 'and '(1 2 3)))


(defun mapcar-acc (fn list acc)
  "Implements mapcar with an accumulator. The supplied function fn takes two
   arguments: (value acc) and returns (values mapped-value acc)."
  (labels ((mapcar-acc-rec (fn list acc-result acc-fn)
             (cond ((null list) (values acc-result acc-fn))
                   (t (multiple-value-bind
                            (mapped-value acc-fn2)
                          (funcall fn (car list) acc-fn)
                        (mapcar-acc-rec fn
                                        (cdr list)
                                        (cons mapped-value acc-result)
                                        acc-fn2))))))
    (multiple-value-bind (mapped-list final-acc)
        (mapcar-acc-rec fn list '() acc)
      (values (nreverse mapped-list) final-acc))))

(export 'mapcar-acc)

(test "mapcar-acc"
      '((2 4 6 8) 4)
      (multiple-value-bind (mapped-list final-acc)
          (mapcar-acc #'(lambda (x acc) (values (* x 2) (1+ acc)))
                      '(1 2 3 4)
                      0)
        (list mapped-list final-acc)))


(defun replace-nth (n list value)
  "Returns a shallow copy of the list with n-th element replaced with the given value."
  (mapcar-acc #'(lambda (x index)
                  (values
                   (if (= index n) value x)
                   (1+ index)))
              list
              0))

(export 'replace-nth)

(test "replace-nth"
      '((x b c)
	(a x c)
	(a b x))
      (mapcar #'(lambda (n) (replace-nth n '(a b c) 'x))
	      '(0 1 2)))


(defun take (n list)
  "Returns the first n items of the list."
  (labels ((take-rec (n list acc)
             (if (or (null list) (<= n 0)) (nreverse acc)
                 (take-rec (1- n) (cdr list) (cons (car list) acc)))))
    (take-rec n list nil)))

(export 'take)

(test "take-1"
      '(1 2 3)
      (take 3 '(1 2 3 4 5)))

(test "take-2"
      '(1 2 3 4 5)
      (take 30 '(1 2 3 4 5)))


(defun drop (n list)
  "Returns the list without the first n items."
  (nthcdr n list))

(proclaim '(inline drop))
(export 'drop)

(test "drop-1"
      '(4 5)
      (drop 3 '(1 2 3 4 5)))

(test "drop-2"
      nil
      (drop 30 '(1 2 3 4 5)))

(test "drop-3"
      nil
      (drop 1 nil))


(defun integer-list (from to &optional (step 1))
  "Returns a list of integers in the range [from, to], according to the specified step."
  (loop for i = from then (+ i step)
     until (> i to)
     collecting i))

(export 'integer-list)

(test "integer-list-1"
      '(1 2 3 4 5)
      (integer-list 1 5))

(test "integer-list-2"
      '(1 4)
      (integer-list 1 5 3))


(defun transpose (matrix)
  (when (find-if-not #'null matrix)
    (cons (mapcar #'first matrix)
          (transpose (mapcar #'rest matrix)))))


(defun zip (&rest lists)
  "Takes a number of lists (a1 a2 .. an) .. (z1 z2 .. zn),
   and produces a single list ((a1 .. z1) (a2 .. z2) .. (an .. zn)).
   If not all lists are of the same length, nils are added when necessary."
  (transpose (reduce #'cons lists :from-end t :initial-value nil)))

(export 'zip)

(test "zip-1"
      '((a1 b1) (a2 b2))
      (zip '(a1 a2) '(b1 b2)))

(test "zip-2"
      '((a1 b1 c1 d1 nil) (a2 b2 c2 nil nil) (a3 nil c3 nil nil))
      (zip '(a1 a2 a3) '(b1 b2) '(c1 c2 c3) '(d1) nil))


(defun unzip (list)
  "Takes a list of type ((a1 a2 .. an) (b1 b2 .. bn) ...)
   and returns ((a1 b1 ...) (a2 b2 ...) .. (an bn ...)."
  (transpose list))

(export 'unzip)

(test "unzip-1"
      '((a1 b1 c1 d1))
      (unzip '((a1) (b1) (c1) (d1))))

(test "unzip-3"
      '((a1 b1 c1 d1) (a2 b2 c2 d2) (a3 b3 c3 d3))
      (unzip '((a1 a2 a3) (b1 b2 b3) (c1 c2 c3) (d1 d2 d3))))

(test "unzip-nil"
      '((a b nil c) (x y z nil) (nil nil nil z))
      (unzip '((a x) (b y) (nil z) (c nil z))))


(defun equal-or-first-null (x y &key (test #'equal))
  "Returns true if x is null, or x and y are equal"
  (or (null x)
      (funcall test x y)))

(export 'equal-or-first-null)

(test "equal-or-first-null"
      '(t t nil)
      (list (equal-or-first-null 'a  'a)
	    (equal-or-first-null nil 'a)
	    (equal-or-first-null 'a nil)))


(defvar *object->sexp-visited-objects* nil)

(defun object->sexp (obj &key suppress-types suppress-properties)
  "Converts arbitrary CLOS objects into s-expressions that can easily be used in tests."
  (if (and (subtypep (type-of obj) 'standard-object)
           (find obj *object->sexp-visited-objects*))
      :recursive-reference
      (let ((*object->sexp-visited-objects* (cons obj *object->sexp-visited-objects*)))
        (cond ((find (type-of obj) suppress-types) :suppressed)
              ((subtypep (type-of obj) 'standard-object)
               (multiple-value-bind (instance slots)
                   (make-load-form-saving-slots obj)
                 (let ((class (cadr (cadadr instance)))
                       (bound-slots (mapcar #'(lambda (s)
                                                (list (second (first (last (second s))))
                                                      (object->sexp (second (third s))
                                                                    :suppress-types suppress-types
                                                                    :suppress-properties suppress-properties)))
                                            (remove 'slot-makunbound (rest slots) :key #'first))))
                   (list* class
                          (sort (remove-if #'(lambda (slot)
                                               (find (first slot) suppress-properties))
                                           bound-slots)
                                #'string< :key (compose #'symbol-name #'first))))))
              ((null obj) nil)
              ((listp obj)
               (mapcar #'(lambda (obj) (object->sexp obj
                                                     :suppress-types suppress-types
                                                     :suppress-properties suppress-properties)) obj))
              (t obj)))))

(export 'object->sexp)


(defmacro nlet (name (&rest vars) &body body)
  "Scheme-style named let. It's a very useful construct for expressing iteration."
  (destructuring-bind (vars vals)
      (unzip vars)
    `(labels ((,name ,vars
                ,@body))
       (,name ,@vals))))

(export 'nlet)

#| nlet uses unzip during macroexpansion, which causes compilation order problems with this test.
(test "nlet"
      3
      (nlet list-length ((list '(1 2 3)) (acc 0))
	(if (null list)
	    acc
	    (list-length (cdr list) (1+ acc)))))
|#

(define-modify-macro adjoinf (val key)
  (lambda (lst val key) (adjoin val lst :key key)))

(export 'adjoinf)


(defmacro make-instance-of-superclass (class superclass &rest rest)
  `(if (subtypep ,class ,superclass)
       (make-instance ,class ,@rest)
       (error "~A is not a subclass of ~A." ,class ,superclass)))

(export 'make-instance-of-superclass)


(defun unquote (list)
  "If list is a quoted list, returns the quoted contents. Otherwise returns nil."
  (when (and (listp list) (eq (first list) 'quote))
    (second list)))

(export 'unquote)

(test "unquote-1"
      'symbol
      (unquote ''symbol))

(test "unquote-2"
      '(a list)
      (unquote ''(a list)))

(test "unquote-3"
      nil
      (unquote 'symbol))

(test "unquote-4"
      nil
      (unquote '(a list)))


(defun group-by (key-fn seq &key (test #'eql))
  "Groups the members of seq according to the value of key-fn for each element.
   The return value consists of pairs (key-value matching-list), where matching-list
   is the list of all values from the original sequence that maps to the given key."
  (let ((hash-table (make-hash-table :test test))
        (result nil))
    (reduce #'(lambda (_ x)
                (declare (ignore _))
                (let ((key (funcall key-fn x)))
                  (if (gethash key hash-table)
                      (append1f (gethash key hash-table) x)
                      (setf (gethash key hash-table) (list x)))))
            seq
            :initial-value nil)
    (maphash #'(lambda (key value)
                 (append1f result (list key value)))
             hash-table)
    result))

(export 'group-by)

(test "group-by"
      '((nil (0 3 6  9))
        (1   (1 4 7 10))
        (2   (2 5 8)))
      (group-by #'(lambda (x) (let ((mod (mod x 3))) (if (= mod 0) nil mod)))
                (integer-list 0 10)))
