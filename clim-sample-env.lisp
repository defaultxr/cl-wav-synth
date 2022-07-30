;;; CL-Wav-Synth -  Manipulate WAV files
;;;
;;; Copyright (C) 2006 Philippe Brochard (hocwp@free.fr)
;;;
;;; #Date#: Sun Sep  3 22:41:01 2006
;;;
;;; **********************************************
;;; The authors grant you the rights to distribute
;;; and use this software as governed by the terms
;;; of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html),
;;; known as the LLGPL.
;;; **********************************************

(in-package :cl-wav-synth-clim)

(define-command-table sample-envelope-command-table)


(defun sample-find-first-point (map-sample)
  (let* ((width (bounding-rectangle-width map-sample))
	 (max-ampl (sample-max-ampl (sample *application-frame*))))
    (when *pointer-documentation-output*
      (window-clear *pointer-documentation-output*)
      (format *pointer-documentation-output* "~&Please, select the first point."))
    (draw-sample-info-init)
    (block processor
      (tracking-pointer (map-sample)
	(:pointer-motion (&key window x y)
			 (declare (ignore window))
			 (draw-sample-info-text map-sample x y width max-ampl))
	(:pointer-button-press (&key event x y)
			       (declare (ignore event))
			       (hide-sample-info-text map-sample)
			       (when *pointer-documentation-output*
				 (window-clear *pointer-documentation-output*))
			       (return-from processor
				 (convert-point-to-pane x y map-sample)))))))

(defun sample-find-second-point (index1 ampl1 map-sample)
  (let* ((width (bounding-rectangle-width map-sample))
	 (max-ampl (sample-max-ampl (sample *application-frame*))))
    (multiple-value-bind (x1 y1)
	(convert-pane-to-point index1 ampl1 map-sample)
      (let ((lx x1) (ly y1))
	(when *pointer-documentation-output*
	  (window-clear *pointer-documentation-output*)
	  (format *pointer-documentation-output* "~&Please, select the second point."))
	(draw-sample-info-init)
	(block processor
	  (tracking-pointer (map-sample)
	    (:pointer-motion (&key window x y)
			     (declare (ignore window))
			     (draw-line* map-sample x1 y1 lx ly :ink +flipping-ink+)
			     (multiple-value-bind (ind am)
				 (convert-point-to-pane x y map-sample)
			       (multiple-value-bind (nx ny)
				   (convert-pane-to-point ind am map-sample)
				 (draw-line* map-sample x1 y1 nx ny :ink +flipping-ink+)
				 (setf lx nx ly ny)))
			     (draw-sample-info-text map-sample x y width max-ampl))
	    (:pointer-button-release (&key event x y)
				     (declare (ignore event))
				     (draw-line* map-sample x1 y1 lx ly :ink +flipping-ink+)
				     (hide-sample-info-text map-sample)
				     (when *pointer-documentation-output*
				       (window-clear *pointer-documentation-output*))
				     (return-from processor
				       (convert-point-to-pane x y map-sample)))))))))




(defun sample-edit-bezier-curve (map-sample index1 ampl1 index2 ampl2)
  (let* ((width (bounding-rectangle-width map-sample))
	 (max-ampl (sample-max-ampl (sample *application-frame*)))
	 (pts (make-array '(4 2)))
	 (edit-pts nil))
    (labels ((init-pts ()
	       (multiple-value-bind (x y)
		   (convert-pane-to-point index1 ampl1 map-sample)
		 (setf (aref pts 0 0) x
		       (aref pts 0 1) y
		       (aref pts 1 0) x
		       (aref pts 1 1) y))
	       (multiple-value-bind (x y)
		   (convert-pane-to-point index2 ampl2 map-sample)
		 (setf (aref pts 2 0) x
		       (aref pts 2 1) y
		       (aref pts 3 0) x
		       (aref pts 3 1) y)))
	     (distance (n x y)
	       (+ (abs (- x (aref pts n 0))) (abs (- y (aref pts n 1)))))
	     (find-near-edit-point (x y)
	       (let ((dist (distance 1 x y)))
		 (setf edit-pts 1)
		 (let ((d (distance 2 x y)))
		   (when (< d dist) (setf edit-pts 2 dist d)))
		 (let ((d (distance 0 x y)))
		   (when (< d dist) (setf edit-pts 0 dist d)))
		 (let ((d (distance 3 x y)))
		   (when (< d dist) (setf edit-pts 3 dist d)))))
	     (draw-bezier-curve ()
	       (with-bezier (x1 y1 x2 y2)
		 (draw-line* map-sample x1 y1 x2 y2 :ink +flipping-ink+))
	       (draw-line* map-sample (aref pts 0 0) (aref pts 0 1)
			   (aref pts 1 0) (aref pts 1 1) :ink +flipping-ink+)
	       (draw-line* map-sample (aref pts 2 0) (aref pts 2 1)
			   (aref pts 3 0) (aref pts 3 1) :ink +flipping-ink+)))
      (when *pointer-documentation-output*
	(window-clear *pointer-documentation-output*)
	(format *pointer-documentation-output* "~&L: Move point; R: Validate"))
      (draw-sample-info-init)
      (init-pts)
      (gen-bezier-from-array pts)
      (draw-bezier-curve)
      (block processor
	(tracking-pointer (map-sample)
	  (:pointer-motion (&key window x y)
			   (declare (ignore window))
			   (draw-sample-info-text map-sample x y width max-ampl)
			   (when edit-pts
			     (draw-bezier-curve)
			     (setf (aref pts edit-pts 0) x
				   (aref pts edit-pts 1) y)
			     (gen-bezier-from-array pts)
			     (draw-bezier-curve)))
	  (:pointer-button-release (&key event x y)
				   (declare (ignore event x y))
				   (setf edit-pts nil))
	  (:pointer-button-press (&key event x y)
				 (cond ((eq (pointer-event-button event) +pointer-left-button+)
					(find-near-edit-point x y))
				       ((eq (pointer-event-button event) +pointer-right-button+)
					(when *pointer-documentation-output*
					  (window-clear *pointer-documentation-output*))
					(return-from processor
					  (let ((acc nil))
					    (dotimes (n 4)
					      (multiple-value-bind (index ampl)
						  (convert-point-to-pane (aref pts n 0) (aref pts n 1) map-sample)
						(push index acc)
						(push ampl acc)))
					    (values-list (nreverse acc))))))))))))




(define-command (com-sample-draw-env-click :name t :menu nil
					   :command-table sample-envelope-command-table)
    ((x 'real) (y 'real))
  (wav::save-undo)
  (with-application-frame (frame)
    (let ((map-sample (find-pane-named frame 'map-sample)))
      (multiple-value-bind (index1 ampl1)
	  (convert-point-to-pane x y map-sample)
	(multiple-value-bind (index2 ampl2)
	    (sample-find-second-point index1 ampl1 map-sample)
	  (setf (sample frame)
		(draw-env-fun (sample frame)
			      (list (list (app-sample-to-time index1) ampl1)
				    (list (app-sample-to-time index2) ampl2)))))))))


(defmacro sample-call-com-click (&body body)
  `(with-application-frame (frame)
    (let ((map-sample (find-pane-named frame 'map-sample)))
      (multiple-value-bind (index1 ampl1)
	  (sample-find-first-point map-sample)
	(multiple-value-bind (x1 y1)
	    (convert-pane-to-point index1 ampl1 map-sample)
	  ,@body)))))

(define-command (com-sample-draw-env :name t :menu "Draw a sample envelope"
				     :command-table sample-envelope-command-table)
    ()
  (sample-call-com-click (com-sample-draw-env-click x1 y1)))


(define-presentation-to-command-translator sample-draw-env
    (blank-area com-sample-draw-env-click wav-clim
                :gesture :sample-draw :echo t
		:documentation "Draw sample envelope"
		:pointer-documentation "Draw sample envelope"
                :tester ((window) (typep window 'sample-pane)))
    (x y)
  (list x y))




(define-command (com-sample-draw-bezier-env-click :name t :menu nil
						  :command-table sample-envelope-command-table)
    ((x 'real) (y 'real))
  (wav::save-undo)
  (with-application-frame (frame)
    (let ((map-sample (find-pane-named frame 'map-sample)))
      (multiple-value-bind (index1 ampl1)
	  (convert-point-to-pane x y map-sample)
	(multiple-value-bind (index2 ampl2)
	    (sample-find-second-point index1 ampl1 map-sample)
	  (multiple-value-bind (i1 a1 i2 a2 i3 a3 i4 a4)
	      (sample-edit-bezier-curve map-sample index1 ampl1 index2 ampl2)
	    (setf (sample frame)
		  (draw-env-bezier (sample frame)
				   (app-sample-to-time i1) a1
				   (app-sample-to-time i2) a2
				   (app-sample-to-time i3) a3
				   (app-sample-to-time i4) a4))))))))


(define-command (com-sample-draw-bezier-env :name t :menu "Draw a sample bezier envelope"
					    :command-table sample-envelope-command-table)
    ()
  (sample-call-com-click (com-sample-draw-bezier-env-click x1 y1)))



(define-presentation-to-command-translator sample-draw-bezier-env
    (blank-area com-sample-draw-bezier-env-click wav-clim
                :gesture :sample-draw-bezier :echo t
		:documentation "Draw a sample bezier envelope"
		:pointer-documentation "Draw a sample bezier envelope"
                :tester ((window) (typep window 'sample-pane)))
    (x y)
  (list x y))




(define-command (com-sample-env-click :name t :menu nil
				      :command-table sample-envelope-command-table)
    ((x 'real) (y 'real))
  (wav::save-undo)
  (with-application-frame (frame)
    (labels ((find-max-ampl (i1 a1 i2 a2)
	       (let ((maxa 0) (maxi i1)
		     (a (/ (- a2 a1) (- i2 i1))))
		 (loop for i from (min i1 i2) to (max i1 i2) do
		       (when (> (abs (get-ampl (sample frame) 0 i)) maxa)
			 (setf maxa (get-ampl (sample frame) 0 i)
			       maxi i)))
		 (let* ((coef (+ (* a (- maxi i1)) a1)))
		   (values (/ (+ (* a (- i1 maxi)) coef) maxa)
			   (/ (+ (* a (- i2 maxi)) coef) maxa))))))
      (let ((map-sample (find-pane-named frame 'map-sample)))
	(multiple-value-bind (index1 ampl1)
	    (convert-point-to-pane x y map-sample)
	  (multiple-value-bind (index2 ampl2)
	      (sample-find-second-point index1 ampl1 map-sample)
	    (multiple-value-bind (coef1 coef2)
		(find-max-ampl index1 ampl1 index2 ampl2)
	      (setf (sample frame)
		    (env-fun (sample frame)
			     (list (list (app-sample-to-time index1) coef1)
				   (list (app-sample-to-time index2) coef2)))))))))))


(define-command (com-sample-env :name t :menu "Set the sample envelope"
				:command-table sample-envelope-command-table)
    ()
  (sample-call-com-click (com-sample-env-click x1 y1)))


(define-presentation-to-command-translator sample-env
    (blank-area com-sample-env-click wav-clim
                :gesture :sample-env :echo t
		:documentation "Set the sample envelope"
		:pointer-documentation "Set the sample envelope"
                :tester ((window) (typep window 'sample-pane)))
    (x y)
  (list x y))


(define-command (com-sample-bezier-env-click :name t :menu nil
					     :command-table sample-envelope-command-table)
    ((x 'real) (y 'real))
  (wav::save-undo)
  (with-application-frame (frame)
    (labels ((find-max-ampl (i1 i2)
	       (let ((maxa 0) (maxi i1))
		 (loop for i from (min i1 i2) to (max i1 i2) do
		       (when (> (abs (get-ampl (sample frame) 0 i)) maxa)
			 (setf maxa (get-ampl (sample frame) 0 i)
			       maxi i)))
		 maxa)))
      (let ((map-sample (find-pane-named frame 'map-sample)))
	(multiple-value-bind (index1 ampl1)
	    (convert-point-to-pane x y map-sample)
	  (multiple-value-bind (index2 ampl2)
	      (sample-find-second-point index1 ampl1 map-sample)
	    (multiple-value-bind (i1 a1 i2 a2 i3 a3 i4 a4)
		(sample-edit-bezier-curve map-sample index1 ampl1 index2 ampl2)
	      (let ((maxa (find-max-ampl (min i1 i2 i3 i4) (max i1 i2 i3 i4))))
		(setf (sample frame)
		      (env-bezier (sample frame)
				  (app-sample-to-time i1) (/ a1 maxa)
				  (app-sample-to-time i2) (/ a2 maxa)
				  (app-sample-to-time i3) (/ a3 maxa)
				  (app-sample-to-time i4) (/ a4 maxa)))))))))))



(define-command (com-sample-bezier-env :name t :menu "Set the sample to a bezier envelope"
				       :command-table sample-envelope-command-table)
    ()
  (sample-call-com-click (com-sample-bezier-env-click x1 y1)))


(define-presentation-to-command-translator sample-bezier-env
    (blank-area com-sample-bezier-env-click wav-clim
                :gesture :sample-env-bezier :echo t
		:documentation "Set the sample to a bezier envelope"
		:pointer-documentation "Set the sample to a bezier envelope"
                :tester ((window) (typep window 'sample-pane)))
    (x y)
  (list x y))





(add-command-table-to-listener 'sample-envelope-command-table)
