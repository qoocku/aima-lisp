;;; File: wumpus.lisp -*- Mode: Lisp -*-

(in-package :aima/agents/environments)

;;;; The Wumpus World Environment

(defclass wumpus-world (grid-environment)
  ((size  :initform (@ 6 6))
   (aspec :initform '(aimless-wumpus-agent))
   (bspec :initform '((at edge wall)
                      (* 1 gold)
                      (* 1 wumpus)
                      (at all (p 0.2 pit)))))
  (:documentation "A dangerous world with pits and wumpuses, and some gold."))

(defclass gold   (object) ((name :initform "$") (size :initform 0.1)))
(defclass pit    (object) ((name :initform "O")))
(defclass arrow  (object) ((name :initform "!") (size :initform 0.01)))
(defclass wumpus (object) ((name :initform "W") (alive? :initform t) (size :initform 0.7)))

;;;; Defining the generic functions

(defmethod update-fn ((env wumpus-world))
  ;; See if anyone died
  (for-each agent in (environment-agents env) do
    (when (find-object-if #'deadly? (object-loc (agent-body agent)) env)
      (kill (agent-body agent))))
  ;; Sounds dissipate
  (for-each object in (grid-environment-objects env) do
    (setf (object-sound object) nil))
  ;; Do the normal thing
  (call-next-method))

(defmethod termination? ((env wumpus-world))
  "End when some agent climbs out, or for the default reason (everyone dead)."
  (or (call-next-method)
      (some #'(lambda (agent)
		(and (equal (op (agent-action agent)) 'climb)
		     (equal (object-loc (agent-body agent))
			    (grid-environment-start env))))
	    (environment-agents env))))

(defmethod performance-measure ((env wumpus-world) agent)
  "Score 1000 for getting the gold, with penalty of 10000 if dead
  and penalty of 1 for each step taken."
  (let ((agent-body (agent-body agent)))
    (- (if (some #'gold-p (object-contents agent-body)) 1000 0)
       (if (object-alive? agent-body) 0 10000)
       (environment-step env))))

(defmethod get-percept ((env wumpus-world) agent)
  "Perceive stench, breeze, glitter, bump, and sound."
  (let ((loc (object-loc (agent-body agent))))
    (list ;; stench breeze glitter bump sound
     (if (find-object-or-neighbor-if #'wumpus-p loc env) 'stench)
     (if (find-object-or-neighbor-if #'pit-p loc env) 'breeze)
     (if (find-object-if #'gold-p loc env) 'glitter)
     (if (object-bump (agent-body agent)) 'bump)
     (some #'object-sound (grid-environment-objects env)))))

(defmethod legal-actions ((env wumpus-world))
  "In the wumpus world, agents can move around, grab gold and shoot arrows."
  '(climb shoot grab release speak forward turn))

(defun deadly? (object)
  "Pits and live wumpuses are deadly."
  (or (pit-p object)
      (and (wumpus-p object) (object-alive? object))))

;;;; Actions

(defmethod climb ((env wumpus-world) agent-body)
  "Climb out of the cave."
  (declare-ignore agent-body env)
  ;; Only effect is to end the game; see termination?
  nil)

(defmethod shoot ((env wumpus-world) agent-body)
  (let ((arrow (find-if #'arrow-p (object-contents agent-body))))
    (when arrow
      (setf (object-contents agent-body)
	    (delete arrow (object-contents agent-body)))
      (propagate-arrow (object-loc agent-body)
		       (object-heading agent-body) env))))

(defun propagate-arrow (loc heading env)
  "An arrow keeps going until it kills something or hits a wall."
  (let ((new-loc (add-locs loc heading)))
    (cond ((find-object-if #'object-alive? new-loc env)
	   (kill (find-object-if #'object-alive? new-loc env)))
	  ((find-object-if #'obstacle-p new-loc env))
	  (t (propagate-arrow new-loc heading env)))))

(defun kill (object)
  "Make the object no longer alive."
  (when (object-alive? object)
    (setf (object-alive? object) nil)
    (setf (object-sound object) 'scream)))
