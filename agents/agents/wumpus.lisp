;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*- Author: Peter Norvig

(in-package :aima/agents)

;;;; Agents for the wumpus world

(defclass wumpus-agent (agent)
  ((body :initform (make-wumpus-agent-body)))
  (:documentation "The default wumpus agent gets an arrow."))

(defclass random-wumpus-agent (wumpus-agent)
  ((program
	:initform #'(lambda (percept)
				  (declare (ignore percept))
				  (random-element '(forward (turn right) (turn left) shoot grab
									release climb))))))

(defclass aimless-wumpus-agent (wumpus-agent)
  ((program
	:initform (let ((plan nil)
					(wumpus-alive? t))
				#'(lambda (percept)
					(destructuring-bind (stench breeze glitter bump sound) percept
					  (when sound
						(setf wumpus-alive? nil))
					  (cond (glitter 'grab)
							(bump (setf plan '((turn right) (turn right) forward))
								  (pop plan))
							(plan (pop plan))
							((or breeze (and wumpus-alive? stench))
							 (setf plan (list (random-element '((turn left) (turn right)))
											  'forward))
							 (pop plan))
							(t (random-element '(forward forward (turn right)
												 (turn left))))))))))
  (:documentation "This agent does the obvious reactive things: grab when there's a glitter,
  and turn and move when there's a bump.  If the wumpus is alive and
  there's a stench, it turns and moves.  Otherwise it wanders aimlessly."))
