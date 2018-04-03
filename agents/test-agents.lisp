;;; -*- Mode: Lisp -*-
;;; File: agents/test.lisp

(in-package :aima/agents/agents)

(import 'aima/agents/algorithms:place-in-container)
(import 'aima/agents/environments:run-environment)
(import 'aima/agents/environments:vacuum-world)
(import 'aima/agents/environments:wumpus-world)

(aima/utilities:deftest agents
  ""
  ((progn
     (in-package :aima/agents/agents)
     (import 'aima/agents/algorithms:place-in-container)
     (import 'aima/agents/environments:run-environment)
     (import 'aima/agents/environments:vacuum-world)
     (import 'aima/agents/environments:make-vacuum-world)
     (import 'aima/agents/environments:wumpus-world)))

  ;; Test agents in the vacuum and wumpus worlds.
  ;; Here is how to run an environment, in this case the vacuum world.
  ;; We specify the maximum number of steps, but that is optional:

  ((run-environment (make-instance 'vacuum-world
                                   :max-steps 10)))

  ;;"You can turn off the display (with :stream nil), and just see the results:"
  ((run-environment (make-vacuum-world :stream nil)))

  #|"You can customize several things, such as the agent(s) in the world."
  "By default, a vacuum-world has a random-agent; we can change that to"
  "a slightly smarter agent with the :ASPEC (Agent SPECification) keyword:"|#
  ((run-environment (make-instance
                     'vacuum-world
                     :stream nil
                     :aspec '(reactive-vacuum-agent))))

  #|"We can change the probability of dirt in each cell using the :CSPEC"
  "keyword (Custom SPECification).  It allows a complex language for"
  "specifying objects and where they go."|#
  ((run-environment (make-instance
                     'vacuum-world
                     :cspec '((at all (P 0.9 dirt)))
                     :max-steps 10)))

  >>>>>>> d7f76ecb744f594b45027b749a27955d2f997e45
  ;;"Finally, we can compare 2 or more agents over a number of trials:"
  ((agent-trials #'(lambda () (make-instance 'vacuum-world))
                 '(reactive-vacuum-agent random-vacuum-agent) :n 10))

  ;;"Now for the wumpus world"
  <<<<<<< HEAD
  ((run-environment (make-instance 'aima/agents/environments::wumpus-world :max-steps 10))))
=======
  ((run-environment (make-instance 'wumpus-world
                                   :max-steps 10))))
>>>>>>> d7f76ecb744f594b45027b749a27955d2f997e45
