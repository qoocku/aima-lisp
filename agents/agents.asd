(require "asdf")
(in-package :asdf-user)

(defsystem "aima/agents" :defsystem-depends-on (:aima)
  :description "AIMA Agents Subsystem"
  :version "0.0.1"
  :author "Damian T. Dobroczy\\'nski <qoocku@gmail.com>"
  :licence "Public Domain"
  ;;; --------------------- packages files -----------------------
  :serial t
  :components ((:module "agents"
				:components ((:file "agent")
							 (:file "vacuum")
							 (:file "wumpus")))

			   (:module "algorithms"
				:components ((:file "grid")))

			   (:module "environments"
				:components ((:file "package")
							 (:file "basic-env")
							 (:file "grid-env")
							 (:file "vacuum")
							 (:file "wumpus")))

			   (:file "test-agents")))
