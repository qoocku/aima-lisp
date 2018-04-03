(ql:quickload "aima") ;;; becosue of custom system class defined there

(in-package :asdf)

(defsystem "aima-agents"
  :class asdf::aima-system
  :description "AIMA Agents Subsystem. Code from Part I: Agents and Environments"
  :version "0.0.1"
  :author "Damian T. Dobroczy\\'nski <qoocku@gmail.com>"
  :licence "Public Domain"
  :depends-on (#:aima #:aima-utilities)
  ;; --------------------- packages files -----------------------
  :serial t
  :components ((:file "agents/package")

               (:module "agents"
                :pathname "agents/agents"
                :components ((:file "agent")
                             (:file "vacuum")
                             (:file "wumpus")))

               (:module "environments"
                :pathname "agents/environments"
                :components ((:file "basic-env")
                             (:file "grid-env")
                             (:file "vacuum")
                             (:file "wumpus")))

               (:module "algorithms"
                :pathname "agents/algorithms"
                :components ((:file "grid")))

               (:module "agents+environments"
                :pathname "agents"
                :components ((:file "agents+environments")))

               (:file "agents/test-agents")))
