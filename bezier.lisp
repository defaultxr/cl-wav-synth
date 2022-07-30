;;; CL-Wav-Synth -  Manipulate WAV files
;;;
;;; Copyright (C) 2006 Philippe Brochard (hocwp@free.fr)
;;;
;;; #Date#: Sun Sep  3 20:07:56 2006
;;;
;;; **********************************************
;;; The authors grant you the rights to distribute
;;; and use this software as governed by the terms
;;; of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html),
;;; known as the LLGPL.
;;; **********************************************

(in-package :cl-user)

(defpackage :bezier
  (:use :common-lisp)
  (:export :gen-bezier
	   :gen-bezier-from-array
	   :with-bezier))

(in-package :bezier)

;;; Taken from OnLisp version by Paul Graham
(defparameter *segs* 20)

(defparameter *du* (/ 1.0 *segs*))

(defparameter *bezier-pts* (make-array (list (1+ *segs*) 2)))


(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s)
		     `(,s (gensym)))
		 syms)
     ,@body))

(defmacro gen-bezier (x0 y0 x1 y1 x2 y2 x3 y3)
  "Fill the *bezier-pts* array with bezier curve points"
  (with-gensyms (gx0 gx1 gy0 gy1 gx3 gy3)
    `(let ((,gx0 ,x0) (,gy0 ,y0)
	   (,gx1 ,x1) (,gy1 ,y1)
	   (,gx3 ,x3) (,gy3 ,y3))
      (let ((cx (* (- ,gx1 ,gx0) 3))
	    (cy (* (- ,gy1 ,gy0) 3))
	    (px (* (- ,x2 ,gx1) 3))
	    (py (* (- ,y2 ,gy1) 3)))
	(let ((bx (- px cx))
	      (by (- py cy))
	      (ax (- ,gx3 px ,gx0))
	      (ay (- ,gy3 py ,gy0)))
	  (setf (aref *bezier-pts* 0 0) ,gx0
		(aref *bezier-pts* 0 1) ,gy0)
	  ,@(loop for n from 1 below *segs*
		  collect (let* ((u (* n *du*))
				 (u^2 (* u u))
				 (u^3 (expt u 3)))
			    `(setf (aref *bezier-pts* ,n 0)
			      (+ (* ax ,u^3)
			       (* bx ,u^2)
			       (* cx ,u)
			       ,gx0)
			      (aref *bezier-pts* ,n 1)
			      (+ (* ay ,u^3)
			       (* by ,u^2)
			       (* cy ,u)
			       ,gy0))))
	  (setf (aref *bezier-pts* *segs* 0) ,gx3
		(aref *bezier-pts* *segs* 1) ,gy3))))))


(defmacro gen-bezier-from-array (points)
  "Same as gen-bezier but from a '(4 2) array"
  `(gen-bezier ,@(loop for i from 0 to 7
		       collect `(aref ,points ,(truncate (/ i 2)) ,(mod i 2)))))


(defmacro with-bezier ((x1 y1 x2 y2) &body body)
  "Apply body on each *bezier-pts* points, binding x1, y1, x2, y2
to each points"
  `(dotimes (n *segs*)
    (let ((,x1 (aref *bezier-pts* n 0))
	  (,y1 (aref *bezier-pts* n 1))
	  (,x2 (aref *bezier-pts* (1+ n) 0))
	  (,y2 (aref *bezier-pts* (1+ n) 1)))
      ,@body)))
