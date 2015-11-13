(ql:quickload "asdf")
(ql:quickload "uiop")
(in-package :asdf)

(defsystem "aima/agents"
  :description "AIMA Agents Subsystem"
  :version "0.0.1"
  :author "Damian T. Dobroczy\\'nski <qoocku@gmail.com>"
  :licence "Public Domain"
  ;; --------------------- packages files -----------------------
  :serial t
  :components ((:module "algorithms"
		:pathname "agents/algorithms"
		:components ((:file "package")
			     (:file "grid")))

	       (:module "agents"
		:pathname "agents/agents"
		:components ((:file "package")
			     (:file "agent")
			     (:file "vacuum")
			     (:file "wumpus")))

	       (:module "environments"
		:pathname "agents/environments"
		:components ((:file "package")
			     (:file "basic-env")
			     (:file "grid-env")
			     (:file "vacuum")
			     (:file "wumpus")))

	       (:file "agents/test-agents"))
  :defsystem-depends-on (#:aima #:aima/utilities))
