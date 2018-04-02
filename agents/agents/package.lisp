(defpackage #:aima/agents/agents
  (:use #:common-lisp
        #:aima/utilities)
  (:export #:agent
           #:agent-score
           #:agent-body
           #:agent-percept
           #:agent-action
           #:agent-name
           ;; ---- agent body
           #:agent-body-alive?
           #:agent-body-name
           #:agent-body-holding
           #:ask-user-agent
           #:ask-user
           #:print-structure
           #:initialize-agent-names))
