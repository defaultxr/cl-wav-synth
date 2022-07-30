;;; CL-Wav-Synth -  Manipulate WAV files
;;;
;;; Copyright (C) 2006 Philippe Brochard (hocwp@free.fr)
;;;
;;; #Date#: Fri Aug 25 14:07:19 2006
;;;
;;; **********************************************
;;; The authors grant you the rights to distribute
;;; and use this software as governed by the terms
;;; of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html),
;;; known as the LLGPL.
;;; **********************************************

(in-package :cl-wav-synth-clim)


(define-command-table build-song-selection-command-table)

(defun selection-find-first-point (map-song)
  (when *pointer-documentation-output*
    (window-clear *pointer-documentation-output*)
    (format *pointer-documentation-output* "~&Please, select the first selection point."))
  (block processor
    (tracking-pointer (map-song)
      (:pointer-motion (&key window x y)
		       (declare (ignore window))
		       (display-text-info-on-position x y map-song))
      (:pointer-button-press (&key event x y)
			     (declare (ignore event))
			     (when *pointer-documentation-output*
			       (window-clear *pointer-documentation-output*))
			     (return-from processor
			       (convert-point-to-pane x y map-song))))))

(defun selection-find-second-point (time1 pos1 map-song)
  (multiple-value-bind (x1 y1)
      (convert-pane-to-point time1 pos1 map-song)
    (let ((lx x1) (ly y1))
      (when *pointer-documentation-output*
	(window-clear *pointer-documentation-output*)
	(format *pointer-documentation-output* "~&Please, select the second selection point."))
      (block processor
	(tracking-pointer (map-song)
	  (:pointer-motion (&key window x y)
			   (declare (ignore window))
			   (draw-rectangle* map-song x1 y1 lx ly :filled nil :ink +flipping-ink+)
			   (draw-rectangle* map-song x1 y1 x y :filled nil :ink +flipping-ink+)
			   (setf lx x ly y)
			   (display-text-info-on-position x y map-song))
	  (:pointer-button-release (&key event x y)
				   (declare (ignore event))
				   (when *pointer-documentation-output*
				     (window-clear *pointer-documentation-output*))
				   (return-from processor
				     (convert-point-to-pane x y map-song))))))))

(defun selection-rectangular (map-song)
  (multiple-value-bind (time1 pos1)
      (selection-find-first-point map-song)
    (multiple-value-bind (time2 pos2)
	(selection-find-second-point time1 pos1 map-song)
      (values time1 pos1 time2 pos2))))

(defun selection-move-rectangular (map-song time1 pos1 time2 pos2 &optional song-sample-selected)
  (multiple-value-bind (x1 y1)
      (convert-pane-to-point time1 pos1 map-song)
    (multiple-value-bind (x2 y2)
	(convert-pane-to-point time2 pos2 map-song)
      (let* ((startx (min x1 x2))
	     (starty (min y1 y2))
	     (lx startx)
	     (ly starty)
	     (width (abs (- x2 x1)))
	     (height (abs (- y2 y1))))
	(labels ((draw-song-selected (dx dy color)
		   (dolist (song-sample song-sample-selected)
		     (multiple-value-bind (time-x pos-y)
			 (convert-pane-to-point (s-time song-sample) (s-pos song-sample) map-song)
		       (let ((length-x (* (s-length song-sample) (song-scaling map-song)))
			     (radius-y (* (song-sample-radius map-song) (song-scaling map-song))))
			 (draw-rectangle* map-song (+ time-x dx) (+ (- pos-y radius-y) dy)
					  (+ (+ time-x length-x) dx) (+ (+ pos-y radius-y) dy)
					  :ink color :filled nil))))))
	  (draw-song-selected 0 0 +red+)
	  (draw-rectangle* map-song lx ly (+ lx width) (+ ly height)
			   :filled nil :ink +red+)
	  (draw-song-selected (- lx startx) (- ly starty) +flipping-ink+)
	  (draw-rectangle* map-song lx ly (+ lx width) (+ ly height)
			   :filled nil :ink +flipping-ink+)
	  (block processor
	    (tracking-pointer (map-song)
	      (:pointer-motion (&key window x y)
			       (declare (ignore window))
			       (draw-rectangle* map-song lx ly (+ lx width) (+ ly height)
						:filled nil :ink +flipping-ink+)
			       (draw-song-selected (- lx startx) (- ly starty) +flipping-ink+)
			       (draw-rectangle* map-song x y (+ x width) (+ y height)
						:filled nil :ink +flipping-ink+)
			       (draw-song-selected (- x startx) (- y starty) +flipping-ink+)
			       (setf lx x ly y)
			       (display-text-info-on-position x y map-song))
	      (:pointer-button-release
	       (&key event x y)
	       (declare (ignore event))
	       (return-from processor
		 (multiple-value-bind (time3 pos3)
		     (convert-point-to-pane x y map-song)
		   (multiple-value-bind (time4 pos4)
		       (convert-point-to-pane (+ x width) (+ y height) map-song)
		     (values time3 pos3 time4 pos4))))))))))))


(add-menu-item-to-command-table 'build-song-selection-command-table "Samples operations" :divider nil)



(define-command (com-song-selection-move-click :name t :menu nil
					       :command-table build-song-selection-command-table)
    ((x 'real) (y 'real))
  (with-application-frame (frame)
    (let ((map-song (find-pane-named frame 'map-song)))
      (multiple-value-bind (time1 pos1)
	  (convert-point-to-pane x y map-song)
	(multiple-value-bind (time2 pos2)
	    (selection-find-second-point time1 pos1 map-song)
	  (multiple-value-bind (time3 pos3 time4 pos4)
	      (selection-move-rectangular map-song time1 pos1 time2 pos2
					  (wav::s-find :time (<= (min time1 time2) wav::time (max time1 time2))
						       :pos (<= (min pos1 pos2) wav::pos (max pos1 pos2))))
	    (let ((dtime (- (min time3 time4) (min time1 time2)))
		  (dpos (- (min pos3 pos4) (min pos1 pos2))))
	      (wav::s-map (:time (<= (min time1 time2) wav::time (max time1 time2))
			   :pos (<= (min pos1 pos2) wav::pos (max pos1 pos2)))
		(incf wav::time dtime)
		(incf wav::pos dpos)))))))))

(defmacro selection-call-com-click (&body body)
  `(with-application-frame (frame)
    (let ((map-song (find-pane-named frame 'map-song)))
      (multiple-value-bind (time1 pos1)
	  (selection-find-first-point map-song)
	(multiple-value-bind (x1 y1)
	    (convert-pane-to-point time1 pos1 map-song)
	  ,@body)))))

(define-command (com-song-selection-move :name t :menu "Move samples in selection"
					 :command-table build-song-selection-command-table)
    ()
  (selection-call-com-click (com-song-selection-move-click x1 y1)))



(define-command (com-song-selection-copy-click :name t :menu nil
					       :command-table build-song-selection-command-table)
    ((x 'real) (y 'real))
  (with-application-frame (frame)
    (let ((map-song (find-pane-named frame 'map-song)))
      (multiple-value-bind (time1 pos1)
	  (convert-point-to-pane x y map-song)
	(multiple-value-bind (time2 pos2)
	    (selection-find-second-point time1 pos1 map-song)
	  (multiple-value-bind (time3 pos3 time4 pos4)
	      (selection-move-rectangular map-song time1 pos1 time2 pos2
					  (wav::s-find :time (<= (min time1 time2) wav::time (max time1 time2))
						       :pos (<= (min pos1 pos2) wav::pos (max pos1 pos2))))
	    (let ((dtime (- (min time3 time4) (min time1 time2)))
		  (dpos (- (min pos3 pos4) (min pos1 pos2))))
	      (s-map (:time (<= (min time1 time2) wav::time (max time1 time2))
			    :pos (<= (min pos1 pos2) wav::pos (max pos1 pos2)))
		(s-copy wav::it :time (+ wav::time dtime)
			:pos (+ wav::pos dpos))))))))))

(define-command (com-song-selection-copy :name t :menu "Copy samples in selection"
					 :command-table build-song-selection-command-table)
    ()
  (selection-call-com-click (com-song-selection-copy-click x1 y1)))


(define-command (com-song-selection-delete-click :name t :menu nil
						 :command-table build-song-selection-command-table)
    ((x 'real) (y 'real))
  (with-application-frame (frame)
    (let ((map-song (find-pane-named frame 'map-song)))
      (multiple-value-bind (time1 pos1)
	  (convert-point-to-pane x y map-song)
	(multiple-value-bind (time2 pos2)
	    (selection-find-second-point time1 pos1 map-song)
	  (s-delete :time (<= (min time1 time2) wav::time (max time1 time2))
		    :pos (<= (min pos1 pos2) wav::pos (max pos1 pos2))))))))

(define-command (com-song-selection-delete :name t :menu "Delete samples in selection"
					   :command-table build-song-selection-command-table)
    ()
  (selection-call-com-click (com-song-selection-delete-click x1 y1)))



(add-menu-item-to-command-table 'build-song-selection-command-table "Tags operations" :divider nil)

(define-command (com-song-selection-add-tags :name t :menu "Add tags in selection"
					     :command-table build-song-selection-command-table)
    ()
  (with-application-frame (frame)
    (let ((map-song (find-pane-named frame 'map-song)))
      (multiple-value-bind (time1 pos1 time2 pos2)
	  (selection-rectangular map-song)
	(let ((new-tags (accept 'form :stream (frame-standard-input frame)
				:prompt "Tags to add")))
	  (s-map (:time (<= (min time1 time2) wav::time (max time1 time2))
		  :pos (<= (min pos1 pos2) wav::pos (max pos1 pos2)))
	    (add-tags wav::it new-tags)))))))

(define-command (com-song-selection-del-tags :name t :menu "Delete tags in selection"
					     :command-table build-song-selection-command-table)
    ()
  (with-application-frame (frame)
    (let ((map-song (find-pane-named frame 'map-song)))
      (multiple-value-bind (time1 pos1 time2 pos2)
	  (selection-rectangular map-song)
	(let ((new-tags (accept 'form :stream (frame-standard-input frame)
				:prompt "Tags to delete")))
	  (s-map (:time (<= (min time1 time2) wav::time (max time1 time2))
		  :pos (<= (min pos1 pos2) wav::pos (max pos1 pos2)))
	    (del-tags wav::it new-tags)))))))

(add-menu-item-to-command-table 'build-song-selection-command-table "Build and play operations" :divider nil)



(define-command (com-song-selection-build-and-play :name t :menu "Build and play the selection"
						   :command-table build-song-selection-command-table)
    ()
  (with-application-frame (frame)
    (let* ((map-song (find-pane-named frame 'map-song))
	   (filename (filename-song->wav (song-sample-filename map-song))))
      (multiple-value-bind (time1 pos1 time2 pos2)
	  (selection-rectangular map-song)
	(format t "Building to ~A~%" filename)
	(build-song-in-interval (namestring filename)
				(s-find :time (<= (min time1 time2) wav::time (max time1 time2))
					:pos (<= (min pos1 pos2) wav::pos (max pos1 pos2)))
				(min time1 time2) (max time1 time2))
	(format t "Playing ~A~%" filename)
	(play (namestring filename))))))




(define-presentation-to-command-translator song-selection-move
    (blank-area com-song-selection-move-click wav-clim
                :gesture :move-selection :echo t
		:documentation "Move samples in selection"
		:pointer-documentation "Move samples in selection"
                :tester ((window) (typep window 'song-pane)))
    (x y)
  (list x y))


(define-presentation-to-command-translator song-selection-copy
    (blank-area com-song-selection-copy-click wav-clim
                :gesture :copy-selection :echo t
		:documentation "Copy samples in selection"
		:pointer-documentation "Copy samples in selection"
                :tester ((window) (typep window 'song-pane)))
    (x y)
  (list x y))


(define-presentation-to-command-translator song-selection-delete
    (blank-area com-song-selection-delete-click wav-clim
                :gesture :delete-selection :echo t
		:documentation "Delete samples in selection"
		:pointer-documentation "Delete samples in selection"
                :tester ((window) (typep window 'song-pane)))
    (x y)
  (list x y))


(define-presentation-to-command-translator song-selection-add-tags
    (blank-area com-song-selection-add-tags wav-clim
                :gesture nil :echo t
		:documentation "Add tags in selection"
                :tester ((window) (typep window 'song-pane)))
    ()
  ())


(define-presentation-to-command-translator song-selection-del-tags
    (blank-area com-song-selection-del-tags wav-clim
                :gesture nil :echo t
		:documentation "Delete tags in selection"
                :tester ((window) (typep window 'song-pane)))
    ()
  ())

(define-presentation-to-command-translator song-selection-build-and-play
    (blank-area com-song-selection-build-and-play wav-clim
                :gesture nil :echo t
		:documentation "Build and play the selection"
                :tester ((window) (typep window 'song-pane)))
    ()
  ())


(add-command-table-to-listener 'build-song-selection-command-table)
