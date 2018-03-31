;;; -*- mode: lisp -*-
;;; Author: Damian T. Dobroczy\\'nski <qoocku@gmail.com>

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (find-package 'alexandria)
    (push :alexandria *features*)))

(uiop:define-package #:aima/utilities
    (:recycle #:aima/utilities)
  (:use #:common-lisp #:aima #+alexandria #:alexandria)
  #+alexandria (:shadow #:rotate
                        #:iota
                        #:copy-array
                        #:copy-hash-table)
  (:export #:while
           #:for
           #-alexandria #:deletef
           #:define-if-undefined
           #:length>1
           #:length=1
           #:random-element
           #-alexandria #:mappend
           #-alexandria :starts-with
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
