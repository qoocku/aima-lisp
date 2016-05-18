;;; -*- Mode: Lisp; Author: Peter Norvig -*-

(in-package :asdf)

(defsystem "aima"
  :author "Peter Norvig"
  :description "Main system for AIMA CL code"
  :version "0.0.1"
  :licence "Public Domain"
  :serial t
  :components ((:file "package")))
