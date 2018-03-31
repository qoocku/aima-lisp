(in-package :asdf)

(defsystem "aima-agents"
  :description "AIMA Agents Subsystem. Code from Part I: Agents and Environments"
  :version "0.0.1"
  :author "Damian T. Dobroczy\\'nski <qoocku@gmail.com>"
  :licence "Public Domain"
  :depends-on (#:aima #:aima-utilities)
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

               (:module "agents+environments"
                :pathname "agents"
                :components ((:file "agents+environments")))

               (:file "agents/test-agents")))
