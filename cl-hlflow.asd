;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; --------------------------------------------------------------------
;;; Common Lisp implementation of the HLFLOW algorithm for maximum flows 
;;; as of Dieter Jungnickel: "Graphs, Networks and Algorithms".
;;;
;;; Copyright (c) 2012, Stefan Mandl. All rights reserved.

;;; --------------------------------------------------------------------


(defpackage #:cl-hlflow-system
  (:use #:asdf #:common-lisp))

(in-package #:cl-hlflow-system)

(defsystem cl-hlflow
  :description "cl-hlflow: Common Lisp implementation of the HLFLOW algorithm"
  :long-description
  "cl-hlflow: Common Lisp implementation of the HLFLOW algorithm for maximum flows as of Dieter Jungnickel: `Graphs, Networks and Algorithms'"
  :version "0.0.1"
  :author "Stefan Mandl <StefanMandl@web.de>"
  :licence "BSD"
  :serial t
  :components
  ((:file "cl-hlflow")
   (:file "cl-hlflow-example")))
