;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; --------------------------------------------------------------------
;;; Common Lisp implementation of the HLFLOW algorithm for maximum flows 
;;; as of Dieter Jungnickel: "Graphs, Networks and Algorithms".
;;;
;;; Copyright (c) 2012, Stefan Mandl. All rights reserved.
;;; --------------------------------------------------------------------

(defpackage #:hlflow-example
  (:use #:cl #:hlflow)
  (:export #:small-1 #:small-1-graph
	   #:small-2 #:small-2-graph
	   #:small-1-from-list-1 #:small-1-from-list-2))

(in-package #:hlflow-example)

(defparameter *small-network*
  (read-network-from-string 
   "6
    0 1 16
    1 0 8
    0 2 13
    2 0 2
    1 2 10
    2 1 3
    1 3 12
    3 1 2
    2 4 14
    4 2 4
    3 2 9
    2 3 1
    3 5 20
    5 3 9
    4 3 7
    3 4 2
    4 5 4
    5 4 1"))

(defparameter *small-network-from-list-1*
  (create-network-from-list
   '(6
     0 1 16
     1 0 8
     0 2 13
     2 0 2
     1 2 10
     2 1 3
     1 3 12
     3 1 2
     2 4 14
     4 2 4
     3 2 9
     2 3 1
     3 5 20
     5 3 9
     4 3 7
     3 4 2
     4 5 4
     5 4 1)))

(defparameter *small-network-from-list-2*
  (create-network-from-list
   '(6
     (0 1 16)
     (1 0 8)
     (0 2 13)
     (2 0 2)
     (1 2 10)
     (2 1 3)
     (1 3 12)
     (3 1 2)
     (2 4 14)
     (4 2 4)
     (3 2 9)
     (2 3 1)
     (3 5 20)
     (5 3 9)
     (4 3 7)
     (3 4 2)
     (4 5 4)
     (5 4 1))))

(defun small-1-from-list-1 ()
  (hlflow *small-network-from-list-1* 0 5))
(defun small-1-from-list-2 ()
  (hlflow *small-network-from-list-2* 0 5))

(defun small-1 ()
  (hlflow *small-network* 0 5))

(defun small-1-graph ()
  (multiple-value-bind (flow graph) (hlflow *small-network* 0 5 :generate-dot-string t)
    (declare (ignore flow))
    graph))

(defun small-2 ()
  (hlflow *small-network* 5 0))


(defun small-2-graph ()
  (multiple-value-bind (flow graph) (hlflow *small-network* 5 0 :generate-dot-string t)
    (declare (ignore flow))
    graph))


