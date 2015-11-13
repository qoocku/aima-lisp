;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*- Author: Peter Norvig

(in-package :aima/agents/agents)

;;;; Some simple agents for the vacuum world

(defstruct random-vacuum-agent
  "A very stupid agent: ignore percept and choose a random action."
  :include agent
  (program #'(lambda (percept)
	       (declare (ignore percept))
	       (random-element
		'(suck forward (turn right) (turn left) shut-off)))))

(defstruct reactive-vacuum-agent
  "When you bump, turn randomly; otherwise mostly go forward, but
  occasionally turn.  Always suck when there is dirt."
  :include agent
  (program #'(lambda (percept)
	       (destructuring-bind (bump dirt home) percept
		 (cond (dirt 'suck)
		       (home (random-element '(shut-off forward (turn right))))
		       (bump (random-element '((turn right) (turn left))))
		       (t (random-element '(forward forward forward
					    (turn right) (turn left)))))))))
