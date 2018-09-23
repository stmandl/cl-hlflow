;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; --------------------------------------------------------------------
;;; Common Lisp implementation of the HLFLOW algorithm for maximum flows 
;;; as of Dieter Jungnickel: "Graphs, Networks and Algorithms".
;;;
;;; Copyright (c) 2012, Stefan Mandl. All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
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


