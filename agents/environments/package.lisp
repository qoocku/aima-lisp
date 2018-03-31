;;; -*- Mode: Lisp -*-

(uiop:define-package #:aima/agents/environments
    (:recycle #:aima #:aima/agents/environments)
  (:use #:common-lisp
        #:aima
        #:aima/utilities
        #:aima/agents/agents)
  (:export #:environment-agents
           #:object-name))
