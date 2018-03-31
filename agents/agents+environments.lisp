(in-package :aima/agents/agents)
(use-package :aima/agents/environments)

(defun initialize-agent-names (env)
  "Name the agents 1, 2, ... if they don't yet have a name."
  (dolist (agnt (environment-agents env))
    (when (null (agent-name agnt))
      (let ((i (+ 1 (position agnt (environment-agents env))))
            (body (agent-body agnt)))
        (setf (agent-name agnt) i)
        (when (and body (null (aima/agents/environments:object-name body)))
          (setf (aima/agents/environments:object-name body) i))))))
