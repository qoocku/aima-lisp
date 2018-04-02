;;; -*- mode: lisp -*-
;;; Author: Damian T. Dobroczy\\'nski <qoocku@gmail.com>

(defpackage #:aima/utilities
  (:use #:common-lisp)

  (:export #:while
           #:for
           #:deletef
           #:define-if-undefined
           #:length>1
           #:length=1
           #:random-element
           #:mappend
           :starts-with
           #:last1
           #:left-rotate
           #:right-rotate
           #:transpose
           #:reuse-cons
           #:xy-p
           #:@
           #:xy-add
           #:xy-equal
           #:xy-distance
           #:xy-between
           #:rotate
           #:inside
           #:infinity
           #:minus-infinity
           #:average
           #:square
           #:sum
           #:between
           #:random-integer
           #:nothing
           #:deftest
           #:test
           #:hyperspec-link
           #:hyperspec-url))
