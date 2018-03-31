;;; -*- mode: lisp -*-
(in-package :asdf)

(defsystem "aima-utilities"
  :description "AIMA Utilities Subsystem.  Basic functions that are loaded every time, and used by many other systems."
  :version "0.0.1"
  :author "Peter Norvig. (ASDF compliance - Damian T. Dobroczy\\'nski <qoocku@gmail.com>)"
  :licence "Public Domain"
  :depends-on (#:aima #:alexandria)
  ;; --------------------- packages files -----------------------
  :serial t
  :components ((:module "utilities"
                :pathname "utilities"
                :components ((:file "package")
                             (:file "hyperspec")
                             (:file "utilities")
                             (:file "binary-tree")
                             (:file "index")
                             (:file "queue")
                             (:file "test-utilities")))))
