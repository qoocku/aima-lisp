;;; -*- mode: lisp -*-
(uiop:define-package #:aima/agents/algorithms
    (:recycle #:aima/agents/algorithms)
  (:use :common-lisp)
  (:export #:grid-contents
           #:move-object-to
           #:place-object
           #:place-in-container
           #:remove-object
           #:find-object-if
           #:find-neighbor-if
           #:find-object-or-neighbor-if
           #:near?
           #:add-locs
           #:substract-locs
           #:heading->string
           #:absolute-loc
           #:offset-loc))
