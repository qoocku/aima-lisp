;;; -*- Mode: Lisp; Author: Peter Norvig -*-

(in-package :asdf)

(defclass aima-system (asdf:system)
  ((examples :initform nil
             :accessor aima-system-examples
             :documentation "List of examples for the AIMA system.")))

(defsystem "aima"
  :class aima-system
  :author "Peter Norvig"
  :description "Main system for AIMA CL code"
  :version "0.0.1"
  :licence "Public Domain"
  :serial t
  :depends-on (#:trivia)
  :components ((:file "package")))
