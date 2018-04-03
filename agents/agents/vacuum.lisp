;;; -*- Mode: Lisp -*- Author: Peter Norvig

(in-package #:aima/agents)

;;;; Some simple agents for the vacuum world

(defclass random-vacuum-agent (agent)
  ()
  (:documentation "A very stupid agent: ignore percept and choose a random action."))

(defclass reactive-vacuum-agent (agent)
  ()
  (:documentation "When you bump, turn randomly; otherwise mostly go forward, but
  occasionally turn.  Always suck when there is dirt."))

(defmethod program ((an-agent random-vacuum-agent) percept)
  (declare (ignore an-agent percept))
  (random-element
   '(suck forward (turn right) (turn left) shut-off)))

(defmethod program ((an-agent reactive-vacuum-agent) percept)
  (declare (ignore an-agent))
  (destructuring-bind (bump dirt home) percept
    (cond (dirt 'suck)
          (home (random-element '(shut-off forward (turn right))))
          (bump (random-element '((turn right) (turn left))))
          (t (random-element '(forward forward forward
                               (turn right) (turn left)))))))
