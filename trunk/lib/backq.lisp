(in-package :com.google.common)

(defvar *custom-backq-readtable* (copy-readtable))
(defvar *inside-backq* nil)


(defun backq-macro (stream char)
  (declare (ignore char))
  (when *inside-backq*
    (error "Nested backquotes are not supported."))
  (let ((*inside-backq* t))
    (list 'backq (read stream t nil t))))

(set-macro-character #\` #'backq-macro nil *custom-backq-readtable*)

(export 'backq)


(defun comma-macro (stream char)
  (declare (ignore char))
  (unless *inside-backq*
    (error "Comma outside of backquote."))
  (let ((next-char (read-char stream t nil t)))
    (list (if (char= next-char #\@)
	      'comma-at
              (prog1 'comma
		(unread-char next-char stream)))
	  (read stream t nil t))))

(set-macro-character #\, #'comma-macro nil *custom-backq-readtable*)

(export 'comma)
(export 'comma-at)


(defmacro with-backq-readtable (&body body)
  `(let ((*readtable* *custom-backq-readtable*))
    ,@body))

(export 'with-backq-readtable)

(test "backq"
      '(backq (a (comma b) (comma-at (c d))))
      (let ((*readtable* *custom-backq-readtable*))
	(with-input-from-string (str "`(a ,b ,@(c d))")
	  (read str))))
