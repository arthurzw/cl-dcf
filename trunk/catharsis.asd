;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage :com.google.catharsis-system (:use :cl :asdf))
(in-package :com.google.catharsis-system)

(defsystem :catharsis
  :name "catharsis"
  :version "0.1"
  :author "Arthur Zwiegincew <estoril@gmail.com>"
  :description "Catharsis"
  :long-description "Allows the definition of multiple domain languages that process a very high-level system definition into a set of programs in various target languages, such as Java, JavaScript, HTML, CSS. It also generates documentation."
  :depends-on (google-common split-sequence)
  :serial t                             ; The dependencies are linear.
  :components ((:file "packages")
               (:module core
                        :serial t
                        :components ((:file "zexp")
                                     (:file "spath")
                                     (:file "env")
                                     (:file "parser-runtime")
                                     (:file "lang-def.gen")
                                     (:file "sexp-parser")
                                     (:file "parser-compiler")
                                     (:file "source-gen")
                                     (:file "protobuf-text-gen")
                                     (:file "xml-gen")
                                     (:file "grammar")))
               (:module lang
                        :serial t
                        :components ((:file "cpp")
                                     (:file "java")
                                     (:file "javascript")))))
