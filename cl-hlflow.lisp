;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; --------------------------------------------------------------------
;;; Common Lisp implementation of the HLFLOW algorithm for maximum flows 
;;; as of Dieter Jungnickel: "Graphs, Networks and Algorithms".
;;;
;;; Copyright (c) 2012, Stefan Mandl. All rights reserved.
;;; --------------------------------------------------------------------


#|
Networks are typcally read from files or strings.
There, all nodes are indexed by integers and the
format is the following:

n : Number of nodes
u v c : edge from u to v with capacity c
....

|#


(defpackage #:hlflow
  (:use #:cl)
  (:export #:benchmark #:hlflow 
	   #:read-network-from-string #:read-network-from-file
	   #:create-network-from-list))

(in-package #:hlflow)


(declaim (optimize (speed 0) 
		   (safety 3) 
		   (debug 3)))

;; ---------------------------------------------
;; factories for specialiced matrixes and arrays
;; ---------------------------------------------

(defun make-fixnum-square-matrix (n)
  (declare (fixnum n))
  (make-array (list n n)
 	      :initial-element 0
 	      :element-type 'fixnum
 	      :adjustable nil))


(defun make-fixnum-array (n)
  (declare (fixnum n))
  (make-array (list n)
	      :initial-element 0
	      :element-type 'fixnum
	      :adjustable nil))

(declaim (inline make-fixnum-array make-fixnum-square-matrix))

;; -----------------------------------------
;; a data structure for networks
;; -----------------------------------------

(defstruct network
  (num-nodes (error "missing :num-nodes") :type fixnum)
  (capacities (error "missing :capacities") :type (simple-array fixnum (* *)))
  (incidents (error "missing :incidents") :type (simple-array list (*))))


;; -----------------------------------------
;; read the network definition from a stream
;; -----------------------------------------

(defun read-network-from-string (s)
  (with-input-from-string (in s)
    (read-network in)))

(defun read-network-from-file (path)
  (with-open-file (in path :direction :input)
    (read-network in)))

(defun read-network (in)
  "Parses the network definition from a stream"
  ;; read dimension first
  (let ((n (the fixnum (read in))))
    ;; create array
    (let ((e (make-fixnum-square-matrix n))
	  (i (make-array (list n) :initial-element nil :element-type 'list :adjustable nil)))
      (declare (type (simple-array fixnum (* *)) e))
      ;; read edge capacities
      (loop for from = (the fixnum (read in nil 0))
	 for to = (the fixnum (read in nil 0))
	 for capacity = (the fixnum (read in nil 0))
	 while (> capacity 0)
	 do (assert (and (>= from 0) (< from n)))
	    (assert (and (>= to 0) (< to n)))
	    (assert (>= capacity 0))
	    (setf (aref e from to) capacity)
	    (push to (aref i from)))
      (make-network :num-nodes n :capacities e :incidents i))))

(defun create-network-from-list (list)
  "
Creates a flow network from a specification given in list form,
hence 
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
    5 4 1)
For sake of readability, the weights can also be given in the following form:
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
    (5 4 1))
"
  (let* ((n (car list))
	 (e (make-fixnum-square-matrix n))
	 (i (make-array (list n) :initial-element nil :element-type 'list :adjustable nil)))
      (declare (type (simple-array fixnum (* *)) e))
    (labels ((process-links (list)
	       (when (consp list)
		 (let (from to c next)
		   (if (consp (car list))
		       (setf from (caar list)
			     to (cadar list)
			     c (caddar list)
			     next (cdr list))
		       (setf from (car list)
			     to (cadr list)
			     c (caddr list)
			     next (cdddr list)))
		   (assert (and (>= from 0) (< from n)))
		   (assert (and (>= to 0) (< to n)))
		   (assert (>= c 0))
		   (setf (aref e from to) c)
		   (push to (aref i from))
		   (process-links next)))))
	     (process-links (cdr list))
      (make-network :num-nodes n :capacities e :incidents i))))
		   

(declaim (optimize (speed 3) 
		   (safety 0) 
		   (debug 0) 
		   (compilation-speed 0) 
		   (space 0)))

;; --------------------------------------------------
;; a crude priority queue using a heap data structure
;; but as we know the maximum numbers to store in
;; the queue we might as well use storage of a
;; fixed size.
;; --------------------------------------------------


(defstruct bounded-heap
  (max 0 :type fixnum)
  (fill 0 :type fixnum)
  (data nil :type (simple-array fixnum (*))))

(declaim (inline make-bounded-heap))

(defun make-bounded-prioriy-queue (max)
  (declare (fixnum max))
  (make-bounded-heap
   :max max
   :data (make-fixnum-array (the fixnum (* max 2)))))

(defun heap-parent (i)
  (truncate (the fixnum i) 2))

(defun heap-left-child (i)
  (declare (fixnum i))
  (the fixnum (- (* (1+ i) 2) 1)))

(defun heap-right-child (i)
  (declare (fixnum i))
  (the fixnum (* (1+ i) 2)))

(defun heap-element (i heap)
  (declare (fixnum i)
	   (type bounded-heap heap))
  (aref (bounded-heap-data heap) (1+ (* i 2))))

(defun set-heap-element (i heap e)
  (declare (fixnum i e)
	   (type bounded-heap heap))
  (setf (aref (bounded-heap-data heap) (1+ (* i 2))) e)
  e)

(defun heap-priority (i heap)
  (declare (fixnum i)
	   (type bounded-heap heap))
  (aref (bounded-heap-data heap) (* i 2)))

(defun set-heap-priority (i heap e)
  (declare (fixnum i e)
	   (type bounded-heap heap))
  (setf (aref (bounded-heap-data heap) (* i 2)) e)
  e)

(defsetf heap-priority set-heap-priority)
(defsetf heap-element set-heap-element)

(declaim (inline heap-parent heap-left-child heap-right-child heap-element heap-priority set-heap-priority set-heap-element))

(defun heapify (q i)
  (declare (type bounded-heap q) (fixnum i))
  (loop 
     (let ((l (heap-left-child i))
	   (r (heap-right-child i))
	   (N (- (bounded-heap-fill q) 1))
	   (largest 0))
       (declare (fixnum largest N l r))
       (setf largest (if (and  (<= l N)
			       (>= (heap-priority l q)
				   (heap-priority i q)))
			 l i))
       (when (and (<= r N)
		  (>= (heap-priority r q)
		      (heap-priority largest q)))
	 (setf largest r))
       (if (/= largest i)
	   (progn 
	     (rotatef (heap-priority i q)
		      (heap-priority largest q))
	     (rotatef (heap-element i q)
		      (heap-element largest q))
	     (setf i largest))
	   (return)))))


(declaim (inline heapify))			 

;; always handy ...
(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))

(defun queue-insert (e p q)
  (declare (type bounded-heap q) (fixnum e p))
  (assert (< (bounded-heap-fill q) (bounded-heap-max q)))
  (setf (heap-element (bounded-heap-fill q) q) e)
  (setf (heap-priority (bounded-heap-fill q) q) p)
  (incf (bounded-heap-fill q))
  (let ((i (1- (bounded-heap-fill q))))
    (declare (fixnum i))
    (while (and (> i 0)
		(< (heap-priority (heap-parent i) q) p))
      (rotatef (heap-element (heap-parent i) q)
	       (heap-element i q))
      (rotatef (heap-priority (heap-parent i) q)
	       (heap-priority i q))
      (setf i (heap-parent i))))
  q)

(declaim (inline queue-insert))      

(defun queue-size (q)
  (declare (type bounded-heap q))
  (bounded-heap-fill q))

(declaim (inline queue-size))


(defun queue-pop (q)
  (declare (type bounded-heap q))
  (let ((res (heap-element 0 q)))
    (setf (heap-element 0 q) (heap-element (the fixnum (1- (bounded-heap-fill q))) q)
	  (heap-priority 0 q) (heap-priority (the fixnum (1- (bounded-heap-fill q))) q))
    (decf (bounded-heap-fill q))
    (heapify q 0)
    res))

(declaim (inline queue-pop))


(defun queue-contains (e q)
  (declare (fixnum e) (type bounded-heap q))
  (loop for i of-type fixnum from 1 below (* 2 (bounded-heap-fill q)) by 2
     when (= (aref (bounded-heap-data q) i) e)
     do (return-from queue-contains T))
  nil)

(declaim (inline queue-contains))

;; -----------------------------------
;; ok, finally, the max flow algorithm
;; -----------------------------------

(defun maxflow-push (f e r v w)
  "Helper function used by hlflow."
  (declare (type (simple-array fixnum (*)) e)
	   (type (simple-array fixnum (* *)) r f)
	   (fixnum v w))
  (let ((delta (min (aref e v) (aref r v w))))
    (declare (fixnum delta))
    (incf (aref f v w) delta)
    (decf (aref f w v) delta)
    (decf (aref r v w) delta)
    (incf (aref r w v) delta)
    (decf (aref e v) delta)
    (incf (aref e w) delta)))


(defun maxflow-relabel (num-vertices d r v)
  "Helper function used by hlflow."
  (declare (type (simple-array fixnum (*)) d)
	   (type (simple-array fixnum (* *)) r)
	   (fixnum num-vertices v))
  (setf (aref d v)
	(the fixnum 
	  (loop for w of-type fixnum from 0 below num-vertices
	     when (> (aref r v w) 0)
	     minimizing (1+ (aref d w)) into min
	     finally (return min)))))

(declaim (inline maxflow-push maxflow-relabel))



(defun hlflow (n source sink &key (generate-dot-string nil))
  "The hlflow algorithm: find the maximum flow from source to sink in the flow network n."
  (declare (type network n) (fixnum source sink))
  (let* ((num-vertices (network-num-nodes n))
	 (c (network-capacities n))
	 (incidents (network-incidents n))
	 (current-edges (make-array (list num-vertices) :element-type 'list :adjustable nil)))
    (declare (fixnum num-vertices)
	     (type (simple-array fixnum (* *)) c)
	     (type (simple-array list) incidents current-edges))
    (let ((e (make-fixnum-array num-vertices))
	  (d (make-fixnum-array num-vertices))
	  (f (make-fixnum-square-matrix num-vertices))
	  (r (make-fixnum-square-matrix num-vertices))
	  (Q (make-bounded-prioriy-queue (* 1 num-vertices))))
      (declare (type (simple-array fixnum (*)) e d)
	       (type (simple-array fixnum (* *)) f r)
	       (type bounded-heap Q))

      ;; initialize
      (loop for v of-type fixnum from 0 below num-vertices
	 do (loop 
	       for w of-type fixnum from 0 below num-vertices
	       when (and (not (= v source)) (not (= w source)))
	       do (setf (aref r v w) (aref c v w))))
      (setf (aref d source) num-vertices)
      (loop for v of-type fixnum from 0 below num-vertices
	 when (/= v source)
	 do (progn
	      (setf (aref f source v) (aref c source v)
		    (aref r source v) 0
		    (aref f v source) (- (aref c source v))
		    (aref r v source) (+ (aref c v source) (aref c source v))
		    (aref d v) 0
		    (aref e v) (aref c source v)
		    (aref current-edges v) (aref incidents v))
	      (when (and (> (aref e v) 0) (/= v sink))
		(queue-insert v (aref d v) Q))
	      ))

      ;; iterate
      (while (> (queue-size Q) 0)
	(let ((v (queue-pop Q))
	      (rel nil))
	  (declare (fixnum v)
		   (type boolean rel))
	  (loop 
		do
		(let ((w (first (aref current-edges v))))
		  (declare (fixnum w))
		  (when (and (> (aref r v w) 0) (= (aref d v) (1+ (aref d w))))
		    (maxflow-push f e r v w)
		    (when (and (not (queue-contains w Q))
			       (/= w source)
			       (/= w sink))
		      (queue-insert w (aref d w) Q)))
		  (when (> (aref e v) 0)
		    (if (second (aref current-edges v))
			(progn
			  (pop (aref current-edges v)))
			(progn
			  (maxflow-relabel num-vertices d r v)
			  (setf rel T)
			  (setf (aref current-edges v)
				(aref incidents v))))))
		until (or (= (aref e v) 0) rel))
	  (when (> (aref e v) 0)
	    (queue-insert v (aref d v) Q))))
      (if generate-dot-string
	  (values (output-max-flow f num-vertices sink)
		  (output-max-flow-graph f c num-vertices source sink))
	  (output-max-flow f num-vertices sink)))))


;;;
(defun output-max-flow-graph (f c num-vertices source sink)
  (declare (type (simple-array fixnum (* *)) c f)
	   (fixnum num-vertices source sink))
  (with-output-to-string (out)
    (format out "digraph max_flow {~%")
    (format out "~t rankdir=LR;~%")
    (format out "~t node [shape = doublecircle]; N~a N~a~%" source sink)
    (format out "~t node [shape = circle];~%")
    (loop for i of-type fixnum from 0 below num-vertices
	  do (loop for j of-type fixnum from 0 below num-vertices
		   when (> (aref f i j) 0)
		   do (format out "~t N~a -> N~a [ label = \"~a/~a\" ];~%"
			      i j (aref f i j) (aref c i j))))
        (format out "}~%")))

(defun output-max-flow (f num-vertices sink)
  "Compute the flow from source to sink in the flow network f with capacities c."
  (declare (type (simple-array fixnum (* *)) f)
	   (fixnum num-vertices sink))
  (loop for i of-type fixnum from 0 below num-vertices
     summing (aref f i sink) into flow of-type fixnum
     finally (return flow)))



(defun generate-random-flow-network (size &optional (max-cap 30))
  "Generates a random flow network with size nodes where edges have :max-cap maximum capacity."
  (let ((c (make-fixnum-square-matrix size))
	(incidents (make-array (list size) :initial-element nil :element-type 'list :adjustable nil)))
    (declare (fixnum size max-cap))
    (loop for i of-type fixnum from 0 below size
       do (loop for j of-type fixnum from i below size
	     when (/= i j)
	     do (progn
		  (setf (aref c i j) (coerce (random max-cap) 'fixnum))
		  (when (and (> (aref c i j) 0) (/= j 0))
		    (setf (aref c j i) (1+ (coerce (random (the fixnum (1- max-cap))) 'fixnum))))
		  (when (> (aref c i j) 0)
		    (push j (aref incidents i)))
		  (when (> (aref c j i) 0)
		    (push i (aref incidents j)))
		  )))
    (make-network :num-nodes size :capacities c :incidents incidents)))


(defun write-random-flow-network (filename size &key (max-cap 30))
  "Generates a new file with name filename that contains a random flow network definition with size nodes, where
edges have :max-cap maximum capacity."
  (let ((n (generate-random-flow-network size max-cap)))
    (with-open-file (out filename :direction :output :if-exists :supersede)
      (declare (type stream out))
      (format out "~a~%" size)
      (loop for i of-type fixnum from 0 below size
	 do (loop for j of-type fixnum from 0 below size
	       when (> (aref (network-capacities n) i j) 0)
	       do
		 (format out "~a ~a ~a~%" i j (aref (network-capacities n) i j)))))))

(defun benchmark (min-size max-size &key (times 1) (max-cap 30))
  "Runs the hflow algorithm on random networks between min-size and max-size, averaging over :times runs per size,
using :max-cap as maximum edge capacity for the random networks. The functions returns a list of the benchmark results."
  (declare (fixnum min-size max-size times max-cap))
  (loop for size from min-size upto max-size
	collect (let ((acc 0))
		  (format t "~%Size ~a: " size)
		  (loop repeat times		     
			do
			(format t ".") (force-output) 
			(let ((net (generate-random-flow-network size max-cap)))
			  (let ((before (get-internal-real-time)))
			    (hlflow net 0 (1- size))
			    (incf acc (- (get-internal-real-time) before)))))
		  (coerce (/ (/ acc times) internal-time-units-per-second) 'double-float))
	into seconds
	finally (return seconds)))




