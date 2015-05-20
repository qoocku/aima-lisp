;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*- Author: Peter Norvig

(uiop:define-package :aima/agents
	(:use :common-lisp :aima/utilities)
  (:export #:ask-user-agent
		   #:ask-user
		   #:print-structure
		   #:initialize-agent-names))

;;; An agent is something that perceives and acts.  As such, each agent has a
;;; slot to hold its current percept, and its current action.  The action
;;; will be handed back to the environment simulator to perform (if legal).
;;; Each agent also has a slot for the agent program, and one for its score
;;; as determined by the performance measure.
(defclass agent ()
  ((program :initform #'nothing :reader agent-program)	; fn: percept -> action
   (body    :initform (make-agent-body) :reader agent-body)
   (score   :initform 0   :reader agent-score)
   (percept :initform nil :reader agent-percept)
   (action  :initform nil :reader agent-action)
   (name    :initform nil :reader agent-name))

  (:documentation "Agents take actions (based on percepts and the agent program) and receive
  a score (based on the performance measure).  An agent has a body which can
  take action, and a program to choose the actions, based on percepts."))


;;;; Definition of basic AGENT functions

(defclass ask-user-agent (agent)
  ((program :initform 'ask-user))
  (:documentation "An agent that asks the user to type in an action."))


(defun ask-user (percept)
  "Ask the user what action to take."
  (format t "~&Percept is ~A; action? " percept)
  (read))

(defmethod print-structure ((agent agent) stream)
  "Agents are printed by showing their name (or body) and score."
  (format stream "[~A = ~D]" (or (agent-name agent) (agent-body agent))
	  (agent-score agent)))

(defun initialize-agent-names (env)
  "Name the agents 1, 2, ... if they don't yet have a name."
  (for each agent in (environment-agents env) do
       (when (null (agent-name agent))
	 (let ((i (+ 1 (position agent (environment-agents env))))
	       (body (agent-body agent)))
	   (setf (agent-name agent) i)
	   (when (and body (null (object-name body)))
	     (setf (object-name body) i))))))

;; Design Decision Notes

;; We have decided that the agent and its body are two separate objects.
;; We could have combined the agent and its body into one object.  But then
;; each new type of agent would need to inherit from both AGENT and some
;; body type, such as OBJECT.  This would require multiple inheritance,
;; which is part of CLOS, but is not in our simple implementation for those
;; who don't have CLOS.  In any case, it would get messy.  We think that
;; separating agents from agent-bodies is a good thing for this
;; implementation.  (Just don't take it too far and assume that this says
;; anything about the mind-body problem.)
;;
;; We could have defined the agent program as a generic function on the
;; agent.  But that would mean that everytime you want to try out a
;; slightly different agent program, you would need to define a new type.  You
;; would also need to hold state information in slots rather than in local
;; variables, and we would need to have some kind of discipline to ensure
;; that the slots representing intermediate state could be accessed and
;; modified by the agent program, while the slot representing, say, the score
;; could not.  All in all, a closure (a different one for every agent) is
;; exactly what we want in an agent program: a closure encapsulates local
;; state and behavior, and can access only its arguments and closed-over
;; variables.
