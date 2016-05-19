;;; -*- Mode: Lisp -*-

(in-package :aima/agents/environments)

;;;; The Vacuum World: cleaning up dirt in a grid

(defclass dirt (object)
  ((name :initform "*")
   (size :initform 0.01)))

(defclass vacuum-world (grid-environment)
  ((size  :initform (@ 8 8))
   (aspec :initform '(random-vacuum-agent))
   (cspec :initform '((at all (P 0.25 dirt)))))
  (:documentation "A grid with some dirt in it, and by default a reactive vacuum agent."))

;;;; Defining the generic functions

(defmethod performance-measure ((env vacuum-world) agent)
  "100 points for each piece of dirt vacuumed up, -1 point for each
  step taken, and -1000 points if the agent does not return home."
  (- (* 100 (count-if #'dirt-p (object-contents (agent-body agent))))
     (environment-step env)
     (if (equal (object-loc (agent-body agent))
                (grid-environment-start env))
         0
         1000)))

(defmethod get-percept ((env vacuum-world) agent)
  "Percept is a three-element sequence: bump, dirt and home."
  (let ((loc (object-loc (agent-body agent))))
    (list (if (object-bump (agent-body agent)) 'bump)
	  (if (find-object-if #'dirt-p loc env) 'dirt)
	  (if (equal loc (grid-environment-start env)) 'home))))

(defmethod legal-actions ((env vacuum-world))
  '(suck forward turn shut-off))

;;;; Actions (other than the basic grid actions of forward and turn)

(defmethod suck ((env vacuum-world) agent-body)
  (let ((dirt (find-object-if #'dirt-p (object-loc agent-body) env)))
    (when dirt
      (place-in-container dirt agent-body env))))

(defmethod shut-off ((env environment) agent-body)
  (declare-ignore env)
  (setf (object-alive? agent-body) nil))
