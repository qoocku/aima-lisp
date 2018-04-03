;;; -*- Mode: Lisp -*-

(defpackage #:aima/agents/environments
  (:use #:common-lisp
        #:aima
        #:aima/utilities
        #:aima/agents/algorithms
        #:aima/agents/agents)

  (:export #:environment-agents
           #:run-environment
           #:vacuum-world
           #:wumpus-world
           #:object-name
           #:grid-environment-size
           #:grid-environment-grid))
