(ql:quickload "asdf")
(ql:quickload "uiop")
(in-package :asdf)

(defsystem "aima/utilities"
  :description "AIMA Utilities Subsystem"
  :version "0.0.1"
  :author "Damian T. Dobroczy\\'nski <qoocku@gmail.com>"
  :licence "Public Domain"
  :documentation "Basic functions that are loaded every time, and used by many other systems."
  ;; --------------------- packages files -----------------------
  :serial t
  :components ((:module "utilities"
		:components ((:file "utilities")
			     (:file "binary-tree")
			     (:file "index")
			     (:file "queue")
			     (:file "test-utilities"))))
  :defsystem-depends-on (:aima))
