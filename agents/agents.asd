(in-package :asdf-user)

(defsystem "agents"
  :description "AIMA Agents Subsystem"
  :version "0.0.1"
  :author "Damian T. Dobroczy\\'nski <qoocku@gmail.com>"
  :licence "Public Domain"
  ;;; --------------------- packages files -----------------------
  :serial t
  :components ((:file "agents/agent")
			   (:file "agents/vacuum")
			   (:file "agents/wumpus")
			   (:file "test-agents")))
