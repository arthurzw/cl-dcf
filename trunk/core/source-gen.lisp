(in-package :com.google.catharsis)


(defclass source-file-generator ()
  ;; Configuration (must not change after initialization).
  ((output-file         :initform *standard-output*  :initarg :output-file)
   (max-line-width      :initform 120                :initarg :max-line-width)
   (indent              :initform "  "               :initarg :indent)
   (keyword-overrides   :initform nil                :initarg :keyword-overrides
			:documentation "A list of pairs (keyword, override) that provide custom
                                        interpretations of given keywords.")
   ;; Public state.
   (current-line        :initform 1  :reader current-line)
   (current-col         :initform 1  :reader current-col)
   ;; Internal state.
   (new-line-requested  :initform nil)
   (indentation-level   :initform 0)
   (wrap-marker-stack   :initform '(1)  :documentation "Stack of column numbers indicating where to wrap lines.")
   (space-requested     :initform nil)
   (space-required      :initform nil)
   (pascal-case         :initform nil)
   (upper-case          :initform nil)
   (lower-case          :initform nil)
   (lower-case_         :initform nil)
   (suppress-wrap       :initform nil))
  (:documentation "Generates a source file from a sequence of tokens."))

(export 'source-file-generator)


(defun internal-write (gen str-or-newline)
  (with-slots (output-file current-line current-col) gen
    (if (eql str-or-newline #\newline)
	(progn (incf current-line)
	       (setf current-col 1)
	       (write-char #\newline output-file))
	(progn (incf current-col (length str-or-newline))
	       (write-string str-or-newline output-file)))))


(defun emit-space-required (gen)
  (with-slots (space-requested space-required) gen
    (unless space-requested
      (if space-required
	  (setf space-requested t)
	  (setf space-required t)))))


(defun emit-vertical-space (gen)
  (progn
    (internal-write gen #\newline)
    (emit-new-line gen)))


(defun emit-new-line (gen)
  (with-slots (new-line-requested) gen
    (setf new-line-requested t)))


(defun emit-line-wrap (gen)
  (with-slots (current-col wrap-marker-stack) gen
    (progn
      (when (null wrap-marker-stack)
	(error "Wrap marker stack is empty."))
      ;; Force a new line and indent at the current level.
      (emit-new-line gen)
      (before-emit-token gen)
      ;; Insert enough spaces to move from the current column to the desired indent level.
      (let ((needed-spaces (- (first wrap-marker-stack) current-col)))
	(when (> needed-spaces 0)
	  (emit-token gen
		      (make-string needed-spaces :initial-element #\space)
		      nil nil nil))))))


(defun emit-push-wrap-marker (gen)
  (with-slots (space-required current-col wrap-marker-stack) gen
    (push (+ current-col
	     (if space-required 1 0))
	  wrap-marker-stack)))


(defun emit-pop-wrap-marker (gen)
  (with-slots (wrap-marker-stack) gen
    (pop wrap-marker-stack)))


(defun emit-increase-indent (gen)
  (with-slots (indentation-level) gen
    (incf indentation-level)))


(defun emit-decrease-indent (gen)
  (with-slots (indentation-level) gen
    (decf indentation-level)))


(defun before-emit-token (gen)
  (with-slots (space-requested space-required new-line-requested indent indentation-level) gen
    (progn
      (when space-requested
	(setf space-requested nil)
	(unless new-line-requested
	  (internal-write gen " ")))
      (when new-line-requested
	(setf new-line-requested nil)
	(setf space-requested nil)
	(internal-write gen #\newline)
	(dotimes (i indentation-level)
	  (internal-write gen indent)))
      (setf space-required nil))))


(defun emit-pascal-case (gen)
  (with-slots (pascal-case) gen
    (setf pascal-case t)))


(defun emit-upper-case (gen)
  (with-slots (upper-case) gen
    (setf upper-case t)))


(defun emit-lower-case (gen)
  (with-slots (lower-case) gen
    (setf lower-case t)))


(defun emit-lower-case_ (gen)
  (with-slots (lower-case_) gen
    (setf lower-case_ t)))


(let ((casing-packages (mapcar #'find-package '(:upper-case :lower-case :lower-case_ :pascal-case :camel-case))))
  (defun casing-package-p (symbol)
    (member (symbol-package symbol) casing-packages)))


(defun symbol->pascal-string (symbol)
  (reduce #'string+
	  (mapcar #'string-capitalize
		  (split-sequence:split-sequence #\- (symbol-name symbol)))))

(export 'symbol->pascal-string)


(defun symbol->camel-string (symbol)
  (let ((ps (symbol->pascal-string symbol)))
    (setf (elt ps 0)
          (char-downcase (elt ps 0)))
    ps))

(export 'symbol->camel-string)


(defun symbol->upper-case-string (symbol)
  (substitute #\_ #\- (symbol-name symbol)))

(export 'symbol->upper-case-string)


(defun symbol->lower-case-string (symbol)
  (string-downcase (symbol->upper-case-string symbol)))

(export 'symbol->lower-case-string)


(defun symbol->lower-case_-string (symbol)
  (concatenate 'string (string-downcase (symbol->upper-case-string symbol)) "_"))

(export 'symbol->lower-case_-string)


(defun emit-suppress-wrap (gen)
  (with-slots (suppress-wrap) gen
    (setf suppress-wrap t)))


(defun emit-token (gen token &optional (space-required-before t) (space-required-after t) (may-wrap t))
  (with-slots (keyword-overrides current-col max-line-width
                                 space-required pascal-case upper-case lower-case lower-case_ suppress-wrap) gen
    (progn

      ;; Apply suppress-wrap.
      (when suppress-wrap
        (setf may-wrap nil)
        (setf suppress-wrap nil))

      ;; If a keyword is specified, convert it to an appropriate string.
      (when (keywordp token)
        (aif (find token keyword-overrides :key #'first)
             (setf token (second it))
             (cond (upper-case (setf token (symbol->upper-case-string token)))
                   (lower-case (setf token (symbol->lower-case-string token)))
                   (lower-case_ (setf token (symbol->lower-case_-string token)))
                   (pascal-case (setf token (symbol->pascal-string token)))
                   (t (setf token (symbol->camel-string token))))))

      ;; Honor explicit casing instructions.
      (when (symbolp token)
        (cond ((eq (symbol-package token) (find-package 'upper-case))
               (setf token (symbol->upper-case-string token)))
              ((eq (symbol-package token) (find-package 'lower-case))
               (setf token (symbol->lower-case-string token)))
              ((eq (symbol-package token) (find-package 'lower-case_))
               (setf token (symbol->lower-case_-string token)))
              ((eq (symbol-package token) (find-package 'pascal-case))
               (setf token (symbol->pascal-string token)))
              ((eq (symbol-package token) (find-package 'camel-case))
               (setf token (symbol->camel-string token)))))

      ;; Reset case toggles because they only potentially applies to the subsequent token.
      (when pascal-case (setf pascal-case nil))
      (when upper-case (setf upper-case nil))
      (when lower-case (setf lower-case nil))
      (when lower-case_ (setf lower-case_ nil))

      ;; If a number is supplied, convert it to a string.
      (when (numberp token)
        (setf token (format nil "~A" token)))

      ;; Check for wrap-around.
      (when (and may-wrap
                 (> (+ current-col (length token) 1) ; The additional 1 accounts for a possible space.
                    max-line-width))
        (emit-line-wrap gen))

      ;; Deal with spaces, and emit the token.
      (when space-required-before (emit-space-required gen))
      (before-emit-token gen)
      (internal-write gen token)
      (when space-required-after (emit-space-required gen)))))


(defun emit-begin-block (gen)
  (with-slots (indentation-level) gen
    (progn
      (emit-token gen "{")
      (incf indentation-level)
      (emit-new-line gen))))


(defun emit-end-block (gen &optional (new-line-after t))
  (with-slots (indentation-level) gen
    (progn
      (decf indentation-level)
      (emit-new-line gen)
      (emit-token gen "}")
      (when new-line-after
	(emit-new-line gen)))))


(defun emit-list (gen &rest item-list)
  "Convenient accessor to all emit-* methods of source-file-generator.
   It automatically calls the right method for each item in item-list."
  (mapc #'(lambda (item)
	    (cond ((listp item)
		   (apply #'emit-token gen item))
		  ((find item '(vertical-space
				new-line
				line-wrap
				push-wrap-marker
				pop-wrap-marker
				pascal-case
				upper-case
				lower-case
				lower-case_
				suppress-wrap
				begin-block
				end-block
				increase-indent
				decrease-indent))
		   (funcall (symbol-function (gen-symbol :catharsis :emit item)) gen))
		  ((eq item 'comma)
		   (emit-token gen "," nil t))
		  ((eq item 'open-paren)
		   (progn (emit-token gen "(" t nil t)
			  (emit-push-wrap-marker gen)))
		  ((eq item 'open-paren-no-space)
		   (progn (emit-token gen "(" nil nil nil)
			  (emit-push-wrap-marker gen)))
		  ((eq item 'close-paren)
		   (progn (emit-token gen ")" nil t nil)
			  (emit-pop-wrap-marker gen)))
		  ((eq item 'close-paren-no-space)
		   (progn (emit-token gen ")" nil nil nil)
			  (emit-pop-wrap-marker gen)))
		  ((eq item 'semicolon)
		   (progn (emit-token gen ";" nil t nil)
			  (emit-new-line gen)))
		  ((eq item 'end-block-no-new-line) (emit-end-block gen nil))
		  (t (emit-token gen item))))
	item-list))

(export 'emit-list)
(dolist (sym '(vertical-space  new-line  line-wrap  push-wrap-marker  pop-wrap-marker
	       pascal-case  upper-case  lower-case  lower-case_  suppress-wrap  begin-block  end-block
	       increase-indent  decrease-indent
	       comma open-paren  open-paren-no-space  close-paren  close-paren-no-space
	       semicolon  end-block-no-new-line))
  (export sym))

;; TODO: support for conditional new-line (like Lisp's ~&).

;;; Test.
(test "source-file-generator"
      "public class ClassName: BaseClass1, BaseClass2 {

  myRandomName(param1,
               param2, veryLongParamXxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx,
               veryLongParamXxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx,
               veryLongParamXxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx, UPPER_CASE_PARAM);
}"
      (with-output-to-string (out)
	(let ((gen (make-instance 'source-file-generator :output-file out)))
	  (emit-list gen
		     "public" "class" 'pascal-case :class-name
		     '(":" nil t) "BaseClass1" 'comma "BaseClass2" 'begin-block
		     'vertical-space
		     :my-random-name 'open-paren-no-space
		     "param1" 'comma 'line-wrap "param2"
		     'comma :very-long-param-xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
		     'comma :very-long-param-xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
		     'comma :very-long-param-xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
		     'comma 'upper-case::upper-case-param
		     'close-paren-no-space
		     'semicolon
		     'new-line ; Test that this redundant new line doesn't do anything.
		     'end-block))))
