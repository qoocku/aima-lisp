;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*- Author: Peter Norvig

(in-package #:aima/agents)

(defclass wumpus-agent-body (agent-body)
  ((contents :initform (list (make-instance 'arrow))))
  (:documentation "The default wumpus agent body is given an arrow."))


;;;; Agents for the wumpus world

(defclass wumpus-agent (agent)
  ((body :initform (make-instance 'wumpus-agent-body)))
  (:documentation "The default wumpus agent gets an arrow."))

(defclass random-wumpus-agent (wumpus-agent) nil)

(defclass aimless-wumpus-agent (wumpus-agent)
  ((plan          :initform nil)
   (wumpus-alive? :initform t))
  (:documentation "This agent does the obvious reactive things: grab when there's a glitter,
  and turn and move when there's a bump.  If the wumpus is alive and
  there's a stench, it turns and moves.  Otherwise it wanders aimlessly."))


(defmethod program ((an-agent wumpus-agent) percept)
  (declare (ignore an-agent percept))
  (aima/utilities:random-element '(forward (turn right) (turn left) shoot grab
                                   release climb)))

(defmethod program ((an-agent aimless-wumpus-agent) percept)
  (with-slots ((plan plan)
               (wumpus-alive? wumpus-alive?)) an-agent
    (destructuring-bind (stench breeze glitter bump sound) percept
      (when sound
        (setf wumpus-alive? nil))
      (cond (glitter 'grab)
            (bump (setf plan '((turn right) (turn right) forward))
                  (pop plan))
            (plan (pop plan))
            ((or breeze (and wumpus-alive? stench))
             (setf plan (list (aima/utilities:random-element '((turn left) (turn right)))
                              'forward))
             (pop plan))
            (t (aima/utilities:random-element '(forward forward (turn right)
                                                (turn left))))))))
