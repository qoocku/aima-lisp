;;; -*- Mode: Lisp -*-

(uiop:define-package #:aima/agents/environments
    (:recycle #:aima #:aima/agents/environments)
  (:use #:common-lisp
        #:aima
        #:aima/utilities
        #:aima/agents/agents)
  (:export #:environment-agents
           #:environment-size
           #:object-name
           #:run-environment
           #:basic-env
           #:vacuum-world
           #:wumpus-world
           #:grid-world))
