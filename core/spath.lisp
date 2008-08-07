(in-package :com.google.catharsis)


(let ((star-package "(find-package '*)"))
  (defun symbol-equal (a b)
    "Compares two symbols.  If either symbol is in the package '*', ignore the package name.
     If neither symbol is in the package '*', compare the two symbol packages and names"
    (if (or (eq star-package (symbol-package a))
	    (eq star-package (symbol-package b)))
	(equal (symbol-name a) (symbol-name b))
	(eq a b))))


(defun spath-item (data-list path-item)
  "Applies a single spath item to each member of the input list. If nothing matches, returns nil.
   The second returned value specifies whether the selection specifies a set."
  ; todo: this is ugly--this routine should operate on data, not data-list.
  ; or embed the case deeper into the main path.
  (case path-item
    (:value
     (values (mapcar #'(lambda (data) (get-zexp-value data))
		     data-list)
	     nil))
    (:name
     (values (mapcar #'(lambda (data) (get-zexp-name data))
		     data-list)
	     nil))
    (t
     (let* ((path-item-str (symbol-name path-item))
	    (set-predicate
	     (char= #\* (last-char path-item-str)))
	    (adjusted-path-item
	     (if set-predicate
		 (intern (subseq path-item-str 0 (1- (length path-item-str)))
			 (symbol-package path-item))
		 path-item)))
       (values
	(apply #'append
	       (mapcar #'(lambda (data)
			   (remove-if-not #'(lambda (data-item)
					      (when (listp data-item)
						(symbol-equal (get-zexp-tag data-item)
							      adjusted-path-item)))
					  data))
		       data-list))
	set-predicate)))))


(defun spath-pred (data-list pred-expr)
  "Applies the S-path predicate pred-expr to all elements of data-list."
  (case (first pred-expr)
    ('= (remove-if-not #'(lambda (data)
			   (remove-if-not #'(lambda (value) (equal value (third pred-expr)))
					  (spath-rec (list data) t (second pred-expr))))
		       data-list))
    (otherwise (remove-if-not #'(lambda (data) (spath-rec (list data) t pred-expr))
			      data-list))))


(defun spath-rec (data-list is-set path)
  (cond ((null path) (values data-list is-set))
	((atom path) (spath-rec data-list is-set (list path)))
	(t
	 (let ((path-item (first path))
	       (rest (rest path)))
	   (cond ((atom path-item)
		  (multiple-value-bind (result new-is-set)
		      (spath-item data-list path-item)
		    (spath-rec result
			       (or is-set new-is-set)
			       rest)))
		 ((listp path-item)
		  (spath-rec (spath-pred data-list path-item)
			     is-set
			     rest)))))))


(defun spath (data path)
  (multiple-value-bind (result is-set)
      (spath-rec (list data) nil path)
    (if is-set result (first result))))

(export 'spath)


(define-constant +test-data+
  '(a :my-a
    (b (bb 0)) (b (bb 1)) (b (bb 2)) (b)
    (c 3 (cc 4))
    (d)))

(test "simple-single-item"
      '(c 3 (cc 4))
      (spath +test-data+ '(c)))

(test "single-value"
      3
      (spath +test-data+ '(c :value)))

(test "simple-multiple-items"
      '((b (bb 0)) (b (bb 1)) (b (bb 2)) (b))
      (spath +test-data+ '(b*)))

(test "multiple-values"
      '(0 1 2)
      (spath +test-data+ '(b* bb :value)))

(test "multiple-items-with-path-filter"
      '((bb 0) (bb 1) (bb 2))
      (spath +test-data+ '(b* bb)))

(test "simple-predicate"
      '(cc 4)
      (spath +test-data+ '(c (= :value 3) cc)))

(test "name"
      :my-a
      (spath +test-data+ :name))

(test "existence-predicate"
      '(c 3 (cc 4))
      (spath +test-data+ '(c (cc))))
