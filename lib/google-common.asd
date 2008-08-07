;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage :com.google.common-system (:use :cl :asdf))
(in-package :com.google.common-system)

(defsystem :google-common
  :name "com.google.common"
  :description "Common Google Lisp utilities."
  :serial t
  :components ((:file "packages")
	       (:file "utils-third-party")
	       (:file "test")
	       (:file "utils")
	       (:file "string")
	       (:file "trie")
	       (:file "backq")
	       #|(:file "system")|#))
