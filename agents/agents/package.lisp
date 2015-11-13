(uiop:define-package :aima/agents/agents
    (:recycle :aima/utilities)
  (:use :common-lisp :aima/utilities)
  (:export #:ask-user-agent
	   #:ask-user
	   #:print-structure
	   #:initialize-agent-names))
