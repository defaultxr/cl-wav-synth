;;; CL-Wav-Synth -  Manipulate WAV files
;;;
;;; Copyright (C) 2006 Philippe Brochard (hocwp@free.fr)
;;;
;;; #Date#: Wed Jan 31 23:03:36 2007
;;;
;;; **********************************************
;;; The authors grant you the rights to distribute
;;; and use this software as governed by the terms
;;; of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html),
;;; known as the LLGPL.
;;; **********************************************

(in-package :cl-wav-synth-clim)

(defmethod convert-point-to-pane (x y (pane song-pane))
  "Convert x, y to time, pos in pane coordinates"
  (values (float (+ (/ x (song-scaling pane)) (song-origine-x pane)))
	  (float (+ (/ y (song-scaling pane)) (song-origine-y pane)))))

(defmethod convert-pane-to-point (time pos (pane song-pane))
  "Convert time, pos to x, y in pane coordinates"
  (values (float (* (- time (song-origine-x pane)) (song-scaling pane)))
	  (float (* (- pos (song-origine-y pane)) (song-scaling pane)))))

(defgeneric grid-point-to-pane (x y pane))
(defgeneric grid-pane-to-point (x y pane))

(defgeneric snap-point-to-pane (x y pane))
(defgeneric snap-pane-to-point (x y pane))


(defun snap (x grid)
  "Return the snaped value of x (in seconds) according to grid"
  (* (round (/ x grid)) grid))

(defmethod grid-point-to-pane (x y (pane song-pane))
  "Convert x, y to the near grid time, pos in pane coordinates"
  (multiple-value-bind (time pos)
      (convert-point-to-pane x y pane)
    (values (snap time (song-grid-x pane))
	    (snap pos (song-grid-y pane)))))

(defmethod grid-pane-to-point (time pos (pane song-pane))
  "Convert the near grid time, pos to x, y in pane coordinates"
  (convert-pane-to-point (snap time (song-grid-x pane))
			 (snap pos (song-grid-y pane))
			 pane))


(defmethod snap-point-to-pane (x y (pane song-pane))
  "Convert x, y to the near snap time, pos in pane coordinates"
  (multiple-value-bind (time pos)
      (convert-point-to-pane x y pane)
    (if (song-snap-on? pane)
	(values (snap time (song-snap-x pane))
		(snap pos (song-snap-y pane)))
	(values time pos))))

(defmethod snap-pane-to-point (time pos (pane song-pane))
  "Convert the near time, pos to x, y in pane coordinates"
  (if (song-snap-on? pane)
      (convert-pane-to-point (snap time (song-snap-x pane))
			     (snap pos (song-snap-y pane))
			     pane)
      (convert-pane-to-point time pos pane)))


(make-command-table 'build-song-menu-command-table
		    :errorp nil
		    :menu '(("Undo/Redo" :menu build-song-undo-redo-command-table)
			    ("Mouse selection" :menu build-song-selection-command-table)
			    ("Show Sample" :menu build-song-show-sample-command-table)
			    ("Edit sample" :menu build-song-edit-command-table)
			    ("Sample Operation" :menu build-song-operation-command-table)
			    ("Display" :menu build-song-display-command-table)
			    ("Grid" :menu build-song-grid-command-table)
			    ("Song Recorder" :menu song-recorder-command-table)))



(define-command-table build-song-command-table)


;;; Show sample Command Table
(define-command-table build-song-show-sample-command-table)

(define-command (com-show-song-sample :name t :menu "Sample"
				      :command-table build-song-show-sample-command-table)
    ((song-sample 'song-sample))
  (format t "Time: ~As  Position: ~A  Tags: ~S  Color: ~6,'0X  Lenght: ~As~%"
	  (s-time song-sample)
	  (s-pos song-sample)
	  (s-tags song-sample)
	  (s-color song-sample)
	  (s-length song-sample))
  (format t "  ~S~%" (s-form song-sample))
  (force-output))


(define-command (com-play-sample :name t :menu "Play a sample on song"
				 :command-table build-song-show-sample-command-table)
    ((song-sample 'song-sample))
  (let ((sample (eval-song-sample-form song-sample)))
    (when (sample-p sample)
      (play sample))))


(define-command (com-center-on-song-sample :name t :menu "Center on sample"
					   :command-table build-song-show-sample-command-table)
    ((song-sample 'song-sample))
  (with-application-frame (frame)
    (let ((map-song (find-pane-named frame 'map-song)))
      (setf (song-origine-x map-song) (- (s-time song-sample)
					 (/ (bounding-rectangle-width map-song) (song-scaling map-song) 2)))
      (setf (song-origine-y map-song) (- (s-pos song-sample)
					 (/ (bounding-rectangle-height map-song) (song-scaling map-song) 2))))))

(add-command-table-to-listener 'build-song-show-sample-command-table)


;;; Edit sample command table

(define-command-table build-song-edit-command-table)

(define-command (com-edit-song-sample-time :name t :menu "Time"
					   :command-table build-song-edit-command-table)
    ((song-sample 'song-sample))
  (with-application-frame (frame)
    (let ((new-time (accept 'number :stream (frame-standard-input frame)
			    :prompt "New time" :default (s-time song-sample)
			    :insert-default t)))
      (setf (s-time song-sample) new-time))))
  
(define-command (com-edit-song-sample-form :name t :menu "Form"
					   :command-table build-song-edit-command-table)
    ((song-sample 'song-sample))
  (with-application-frame (frame)
    (let ((new-form (accept 'form :stream (frame-standard-input frame)
			    :prompt "New form" :default (s-form song-sample)
			    :insert-default t)))
      (setf (s-form song-sample) new-form))
    (eval-song-sample-form song-sample)))
  
    
(define-command (com-edit-song-sample-pos :name t :menu "Position"
					  :command-table build-song-edit-command-table)
    ((song-sample 'song-sample))
  (with-application-frame (frame)
    (let ((new-pos (accept 'number :stream (frame-standard-input frame)
			   :prompt "New position" :default (s-pos song-sample)
			   :insert-default t)))
      (setf (s-pos song-sample) new-pos))))
  
    
(define-command (com-edit-song-sample-tags :name t :menu "Tags"
					   :command-table build-song-edit-command-table)
    ((song-sample 'song-sample))
  (with-application-frame (frame)
    (let ((new-tags (accept 'form :stream (frame-standard-input frame)
			    :prompt "New tags" :default (s-tags song-sample)
			    :insert-default t)))
      (setf (s-tags song-sample) new-tags))))

(define-command (com-edit-song-sample-color :name t :menu "Color"
					    :command-table build-song-edit-command-table)
    ((song-sample 'song-sample))
  (with-application-frame (frame)
    (let ((new-color (accept 'form :stream (frame-standard-input frame)
			     :prompt "New color" :default (s-color song-sample)
			     :insert-default t)))
      (setf (s-color song-sample) new-color))))

(define-command (com-edit-song-sample-length :name t :menu "Length"
					     :command-table build-song-edit-command-table)
    ((song-sample 'song-sample))
  (with-application-frame (frame)
    (let ((new-length (accept 'form :stream (frame-standard-input frame)
			      :prompt "New length" :default (s-length song-sample)
			      :insert-default t)))
      (setf (s-length song-sample) new-length))))


(define-command (com-edit-song-sample :name t :menu "Sample"
				      :command-table build-song-edit-command-table)
    ((song-sample 'song-sample))
  (com-edit-song-sample-time song-sample)
  (com-edit-song-sample-form song-sample)
  (com-edit-song-sample-pos song-sample)
  (com-edit-song-sample-tags song-sample)
  (com-edit-song-sample-color song-sample)
  (com-edit-song-sample-length song-sample))

(add-command-table-to-listener 'build-song-edit-command-table)


;;; Sample Operation command table

(define-command-table build-song-operation-command-table)

(defun drag-song-sample (frame song-sample start-x start-y)
  (let* ((map-song (find-pane-named frame 'map-song))
	 (radius (* (song-sample-radius map-song) (song-scaling map-song)))
	 (length (* (s-length song-sample) (song-scaling map-song)))
	 (lx start-x) (ly start-y) (dx 0) (dy 0)
	 (ltext "")
	 (width (bounding-rectangle-width map-song)))
    (labels ((display-text (time pos)
	       (draw-text* map-song ltext width 0 :align-x :right :align-y :top :ink (pane-background map-song))
	       (setf ltext (format nil "~,2F, ~,2F" time pos))
	       (draw-text* map-song ltext width 0 :align-x :right :align-y :top :ink +white+))
	     (draw-sample (x y)
	       (draw-rectangle* map-song (- x dx) (- (- y dy) radius)
				(+ (- x dx) length) (+ (- y dy) radius)
				:ink +flipping-ink+ :filled nil)))
      (if (song-snap-on? map-song)
	  (multiple-value-bind (time pos)
	      (snap-point-to-pane start-x start-y map-song)
	    (multiple-value-bind (sx sy)
		(snap-pane-to-point time pos map-song)
	      (setf lx sx ly sy)))
	  (multiple-value-bind (time-x pos-y)
	      (convert-pane-to-point (s-time song-sample) (s-pos song-sample) map-song)
	    (setf dx (- start-x time-x)
		  dy (- start-y pos-y))))
      (draw-sample lx ly)
      (display-text (s-time song-sample) (s-pos song-sample))
      (multiple-value-bind (x y)
	  (block processor
	    (tracking-pointer (map-song)
	      (:pointer-motion (&key window x y)
			       (declare (ignore window))
			       (draw-sample lx ly)
			       (multiple-value-bind (time pos)
				   (snap-point-to-pane x y map-song)
				 (multiple-value-bind (sx sy)
				     (snap-pane-to-point time pos map-song)
				   (draw-sample sx sy)
				   (setf lx sx ly sy))
				 (display-text time pos)))
	      (:pointer-button-release (&key event x y)
				       (declare (ignore event))
				       (return-from processor (values x y)))))
	(multiple-value-bind (time pos)
	    (snap-point-to-pane (- x dx) (- y dy) map-song)
	  (values time pos))))))

(define-command (com-move-song-sample :name t :menu "Move sample"
				      :command-table build-song-operation-command-table)
    ((song-sample 'song-sample)
     (x 'real) (y 'real))
  (wav::s-save-undo)
  (with-application-frame (frame)
    (multiple-value-bind (time pos)
	(drag-song-sample frame song-sample x y)
      (setf (s-time song-sample) time)
      (setf (s-pos song-sample) pos))
    (com-show-song-sample song-sample)))

    
(define-command (com-copy-song-sample :name t :menu "Copy sample"
				      :command-table build-song-operation-command-table)
    ((song-sample 'song-sample)
     (x 'real) (y 'real))
  (wav::s-save-undo)
  (with-application-frame (frame)
    (multiple-value-bind (time pos)
	(drag-song-sample frame song-sample x y)
      (push (make-instance 'song-sample :time time :form (s-form song-sample)
			   :pos pos :tags (s-tags song-sample)
			   :color (s-color song-sample)
			   :length (s-length song-sample))
	    (song frame)))))



(define-command (com-add-song-sample :name t :menu "Add sample"
				     :command-table build-song-operation-command-table)
    ((time 'number :prompt "Time")
     (pos 'number :prompt "Position"))
  (wav::s-save-undo)
  (with-application-frame (frame)
    (let ((new-sample (make-instance 'song-sample :time time :pos pos)))
      (com-edit-song-sample-form new-sample)
      (com-edit-song-sample-tags new-sample)
      (com-edit-song-sample-color new-sample)
      (com-edit-song-sample-length new-sample)
      (push new-sample (song frame)))))

(define-command (com-delete-song-sample :name t :menu "Delete sample"
					:command-table build-song-operation-command-table)
    ((song-sample 'song-sample))
  (wav::s-save-undo)
  (with-application-frame (frame)
    (setf (song frame) (remove song-sample (song frame)))))
    
(add-command-table-to-listener 'build-song-operation-command-table)    



;;; Build Song Display command table

(define-command-table build-song-display-command-table)

(define-command (com-set-song-scaling :name t :menu t
				      :command-table build-song-display-command-table)
    ((scaling 'number :prompt "Scaling"))
  (with-application-frame (frame)
    (let ((map-song (find-pane-named frame 'map-song)))
      (setf (song-scaling map-song) scaling))))

(define-command (com-set-song-origine :name t :menu t
				      :command-table build-song-display-command-table)
    ((dx 'number :prompt "Time Origine")
     (dy 'number :prompt "Position Origine"))
  (with-application-frame (frame)
    (let ((map-song (find-pane-named frame 'map-song)))
      (setf (song-origine-x map-song) dx)
      (setf (song-origine-y map-song) dy))))

(define-command (com-set-song-sample-radius :name t :menu t
					    :command-table build-song-display-command-table)
    ((radius 'number :prompt "Sample Radius"))
  (with-application-frame (frame)
    (let ((map-song (find-pane-named frame 'map-song)))
      (setf (song-sample-radius map-song) radius))))


(add-command-table-to-listener 'build-song-display-command-table)



;;; Grid command table
(define-command-table build-song-grid-command-table)

(add-menu-item-to-command-table 'build-song-grid-command-table "Grid" :divider nil)

(define-command (com-set-song-grid :name t :menu t
				   :command-table build-song-grid-command-table)
    ((grid-x 'number :prompt "Grid X")
     (grid-y 'number :prompt "Grid Y"))
  (with-application-frame (frame)
    (let ((map-song (find-pane-named frame 'map-song)))
      (setf (song-grid-x map-song) grid-x)
      (setf (song-grid-y map-song) grid-y))))

(define-command (com-song-toggle-grid :name t :menu t
				      :command-table build-song-grid-command-table)
    ()
  (with-application-frame (frame)
    (let ((map-song (find-pane-named frame 'map-song)))
      (setf (song-grid-on? map-song) (not (song-grid-on? map-song))))))


(add-menu-item-to-command-table 'build-song-grid-command-table "Snap" :divider nil)


(define-command (com-set-song-snap :name t :menu t
				   :command-table build-song-grid-command-table)
    ((snap-x 'number :prompt "Snap X")
     (snap-y 'number :prompt "Snap Y"))
  (with-application-frame (frame)
    (let ((map-song (find-pane-named frame 'map-song)))
      (setf (song-snap-x map-song) snap-x)
      (setf (song-snap-y map-song) snap-y))))

(define-command (com-song-toggle-snap :name t :menu t
				      :command-table build-song-grid-command-table)
    ()
  (with-application-frame (frame)
    (let ((map-song (find-pane-named frame 'map-song)))
      (setf (song-snap-on? map-song) (not (song-snap-on? map-song))))))


(add-command-table-to-listener 'build-song-grid-command-table)



;; Building command table
(defun filename-song->wav (path)
  "Convert .song path to .wav path"
  (make-pathname :host (pathname-host path)
		 :device (pathname-device path)
		 :directory (pathname-directory path)
		 :name (pathname-name path)
		 :type "wav"
		 :version (pathname-version path)))

(define-command (com-build-song :name t :menu "Build song"
				:command-table build-song-menu-command-table)
    ()
  (with-application-frame (frame)
    (let* ((map-song (find-pane-named frame 'map-song))
	   (filename (filename-song->wav (song-sample-filename map-song))))
      (format t "Building to ~A~%" filename)
      (build-song (namestring filename) (song frame)))))

(define-command (com-build-song-in-interval :name t :menu "Build song in interval"
					    :command-table build-song-menu-command-table)
    ((begin 'number :prompt "Begining time")
     (end 'number :prompt "End time"))
  (with-application-frame (frame)
    (let* ((map-song (find-pane-named frame 'map-song))
	   (filename (filename-song->wav (song-sample-filename map-song))))
      (format t "Building to ~A~%" filename)
      (build-song-in-interval (namestring filename) (song frame) begin end))))


(define-command (com-play-song :name t :menu "Play song"
			       :command-table build-song-menu-command-table)
    ()
  (with-application-frame (frame)
    (let* ((map-song (find-pane-named frame 'map-song))
	   (filename (filename-song->wav (song-sample-filename map-song))))
      (format t "Playing ~A~%" filename)
      (play (namestring filename)))))

(add-command-table-to-listener 'build-song-menu-command-table)




;;; Zooming functions

(define-presentation-to-command-translator song-sample-center
    (blank-area com-song-sample-center wav-clim
                :gesture :center :echo t
		:documentation "Center"
		:pointer-documentation "Center"
                :tester ((window) (typep window 'song-pane)))
    (x y window)
  (list x y window))


(define-presentation-to-command-translator song-sample-zoom-in
    (blank-area com-song-sample-zoom-in wav-clim
                :gesture :zoom-in :echo t
		:documentation "Zoom In"
		:pointer-documentation "Zoom In"
                :tester ((window) (typep window 'song-pane)))
    (x y window)
  (list x y window))


(define-presentation-to-command-translator song-sample-zoom-out
    (blank-area com-song-sample-zoom-out wav-clim
                :gesture :zoom-out :echo t
		:documentation "Zoom Out"
		:pointer-documentation "Zoom Out"
                :tester ((window) (typep window 'song-pane)))
    (x y window)
  (list x y window))


(define-presentation-to-command-translator song-sample-wheel-zoom-in
    (blank-area com-song-sample-zoom-in wav-clim
                :gesture :wheel-zoom-in :echo t
		:documentation "Zoom In"
		:pointer-documentation "Zoom In"
                :tester ((window) (typep window 'song-pane)))
    (x y window)
  (list x y window))


(define-presentation-to-command-translator song-sample-wheel-zoom-out
    (blank-area com-song-sample-zoom-out wav-clim
                :gesture :wheel-zoom-out :echo t
		:documentation "Zoom Out"
		:pointer-documentation "Zoom Out"
                :tester ((window) (typep window 'song-pane)))
    (x y window)
  (list x y window))



(define-presentation-to-command-translator song-sample-add-sample
    (blank-area com-song-sample-add-sample wav-clim
                :gesture :add-sample :echo t
		:documentation "Add Sample"
		:pointer-documentation "Add Sample"
                :tester ((window) (typep window 'song-pane)))
    (x y window)
  (list x y window))


(define-presentation-to-command-translator song-sample-build-song
    (blank-area com-build-song wav-clim
                :gesture nil :echo t
		:documentation ((object stream)
				(declare (ignore object))
				(let* ((map-song (find-pane-named *application-frame* 'map-song))
				       (filename (filename-song->wav (song-sample-filename map-song))))
				  (format stream "Build Song to ~S" (namestring filename))))
                :tester ((window) (typep window 'song-pane)))
    ()
  nil)

(define-presentation-to-command-translator song-sample-play-song
    (blank-area com-play-song wav-clim
                :gesture nil :echo t
		:documentation ((object stream)
				(declare (ignore object))
				(let* ((map-song (find-pane-named *application-frame* 'map-song))
				       (filename (filename-song->wav (song-sample-filename map-song))))
				  (format stream "Play Song ~S" (namestring filename))))
                :tester ((window) (typep window 'song-pane)))
    ()
  nil)

(define-presentation-to-command-translator song-sample-save-as-song
    (blank-area com-save-as-song wav-clim
                :gesture nil :echo t
		:documentation ((object stream)
				(declare (ignore object))
				(let* ((map-song (find-pane-named *application-frame* 'map-song))
				       (filename (filename-song->wav (song-sample-filename map-song))))
				  (format stream "Save Song ~S" (namestring filename))))
                :tester ((window) (typep window 'song-pane)))
    ()
  (let ((map-song (find-pane-named *application-frame* 'map-song)))
    (list (filename-song->wav (song-sample-filename map-song)))))
    




(let* ((lx 0) (ly 0) (ltext ""))
  (defun display-text-info-on-position (x y map-song)
    (let ((width (bounding-rectangle-width map-song)))
      (multiple-value-bind (time pos)
	  (convert-point-to-pane x y map-song)
	(draw-text* map-song ltext width 0 :align-x :right :align-y :top :ink (pane-background map-song))
	(setf ltext (format nil "~,2Fs, ~,2F (scale=~A)" time pos (float (song-scaling map-song))))
	(draw-text* map-song ltext width 0 :align-x :right :align-y :top :ink +white+)
	(force-output map-song)))
    (setf lx x ly y)))
       

(defun drag-info-on-position (x y map-song)
  (display-text-info-on-position x y map-song)
  (block processor
    (tracking-pointer (map-song)
      (:pointer-motion (&key window x y)
		       (declare (ignore window))
		       (display-text-info-on-position x y map-song))
      (:pointer-button-release (&key event x y)
			       (declare (ignore event))
			       (return-from processor
				 (convert-point-to-pane x y map-song))))))



(define-wav-clim-command (com-song-sample-center :name t :menu nil)
    ((x 'real) (y 'real) (map-song 'song-pane))
  (multiple-value-bind (zx zy)
      (drag-info-on-position x y map-song)
    (setf (song-origine-x map-song) (- zx (/ (bounding-rectangle-width map-song) (song-scaling map-song) 2)))
    (setf (song-origine-y map-song) (- zy (/ (bounding-rectangle-height map-song) (song-scaling map-song) 2)))))



(define-wav-clim-command (com-song-sample-zoom-in :name t :menu nil)
    ((x 'real) (y 'real) (window 'song-pane))
  (multiple-value-bind (zx zy)
      (drag-info-on-position x y window)
    (setf (song-scaling window) (* (song-scaling window) 2))
    (setf (song-origine-x window) (- zx (/ (bounding-rectangle-width window) (song-scaling window) 2)))
    (setf (song-origine-y window) (- zy (/ (bounding-rectangle-height window) (song-scaling window) 2)))))

(define-wav-clim-command (com-song-sample-zoom-out :name t :menu nil)
    ((x 'real) (y 'real) (window 'song-pane))
  (multiple-value-bind (zx zy)
      (drag-info-on-position x y window)
    (setf (song-scaling window) (/ (song-scaling window) 2))
    (setf (song-origine-x window) (- zx (/ (bounding-rectangle-width window) (song-scaling window) 2)))
    (setf (song-origine-y window) (- zy (/ (bounding-rectangle-height window) (song-scaling window) 2)))))


(define-wav-clim-command (com-song-sample-add-sample :name t :menu nil)
    ((x 'real) (y 'real) (window 'song-pane))
  (multiple-value-bind (start-time start-pos)
      (snap-point-to-pane x y window)
    (multiple-value-bind (time pos)
	(drag-song-sample *application-frame*
			  (make-instance 'song-sample :time start-time :pos start-pos)
			  x y)
      (com-add-song-sample time pos))))





(add-command-table-to-listener 'build-song-command-table)


;;; accept method for a song-sample presentation (in any view mode):
(define-presentation-method accept ((type song-sample) stream view &key)
  (declare (ignore view))
  (values
   (completing-from-suggestions (Stream :partial-completers '(#\Space))
     (dolist (sample (song *application-frame*))
       (suggest (format nil "~A ~A " (s-time sample) (s-form sample)) sample)))))


;;;;; Song sample presentation
(define-presentation-method present (song-sample (type song-sample) stream
						 (view textual-view) &key)
  (format stream "~A ~A" (s-time song-sample) (s-form song-sample)))

(define-presentation-method present (song-sample (type song-sample) stream
						 (view graphical-view) &key)
  (multiple-value-bind (time-x pos-y)
      (convert-pane-to-point (s-time song-sample) (s-pos song-sample) stream)
    (let ((length-x (* (s-length song-sample) (song-scaling stream)))
	  (radius-y (* (song-sample-radius stream) (song-scaling stream))))
      (when (and (>= (+ time-x length-x) 0)
		 (<= time-x (bounding-rectangle-width stream))
		 (>= (+ pos-y radius-y) 0)
		 (<= (- pos-y radius-y) (bounding-rectangle-width stream)))
	(draw-rectangle* stream time-x (- pos-y radius-y)
			 (+ time-x length-x) (+ pos-y radius-y)
			 :ink (make-rgb-color (/ (ldb (byte 8 16) (s-color song-sample)) #xFF)
					      (/ (ldb (byte 8 8) (s-color song-sample)) #xFF)
					      (/ (ldb (byte 8 0) (s-color song-sample)) #xFF))
			 :filled t)
	(draw-rectangle* stream time-x (- pos-y radius-y)
			 (+ time-x length-x) (+ pos-y radius-y)
			 :ink +black+ :filled nil)))))




;;; Drawing function
(defun draw-song (frame pane)
  (let ((width (bounding-rectangle-width pane))
	(height (bounding-rectangle-height pane)))
    (labels ((display-axes ()
	       (multiple-value-bind (time-0 pos-0) (convert-pane-to-point 0 0 pane)
		 (when (<= 0 time-0 width)
		   (draw-line* pane time-0 0 time-0 height :ink +yellow+))
		 (when (<= 0 pos-0 height)
		   (draw-line* pane 0 pos-0 width pos-0 :ink +yellow+))))
	     (draw-grid-x (time)
	       (let ((px (convert-pane-to-point time 0 pane)))
		 (when (<= 0 px width)
		   (draw-line* pane px 0 px height
			       :ink +grey50+)
		   (draw-text* pane (format nil "~,2F" time)
			       px height :align-x :center :align-y :bottom
			       :ink +black+))))
	     (draw-grid-y (pos)
	       (multiple-value-bind (px py) (convert-pane-to-point 0 pos pane)
		 (declare (ignore px))
		 (when (<= 0 py height)
		   (draw-line* pane 0 py width py
			       :ink +grey50+)
		   (draw-text* pane (format nil "~,2F" pos)
			       0 py :align-x :left :align-y :bottom
			       :ink +black+))))
	     (display-grid ()
	       (multiple-value-bind (tx0 py0) (grid-point-to-pane 0 0 pane)
		 (multiple-value-bind (txw pyh) (grid-point-to-pane width height pane)
		   (when (>= (* (song-grid-x pane) (song-scaling pane))
			     *song-time-grid-details*)
		     (loop for time from 0 by (song-grid-x pane) to (1+ txw) do
			   (draw-grid-x time))
		     (loop for time from 0 by (song-grid-x pane) downto (1- tx0) do
			   (draw-grid-x time)))
		   (when (>= (* (song-grid-y pane) (song-scaling pane)) *song-pos-grid-details*)
		     (loop for pos from 0 by (song-grid-y pane) to (1+ pyh) do
			   (draw-grid-y pos))
		     (loop for pos from 0 by (song-grid-y pane) downto (1- py0) do
			   (draw-grid-y pos)))))))
      (when (song-grid-on? pane) 
	(display-grid))
      (display-axes))
    (dolist (sample (song frame))
      (present sample 'song-sample :stream pane))))



(defmacro define-song-sample-presentation (name gesture documentation &optional pointer-documentation)
  `(define-presentation-to-command-translator ,name
    (song-sample ,(intern (concatenate 'string "COM-" (string name))) wav-clim
     :gesture ,gesture
     :documentation ,documentation
     :pointer-documentation ,pointer-documentation)
    (object)
    (list object)))

(define-song-sample-presentation center-on-song-sample nil
  "Center on this sample.")

(define-song-sample-presentation show-song-sample :show-sample
  "Show this sample."
  ((object stream)
   (format stream "Show ~A ~A    " (s-time object) (s-form object))))

(define-song-sample-presentation play-sample :play-sample
  "Play this sample."
  ((object stream)
   (format stream "Play ~A ~A    " (s-time object) (s-form object))))

(define-song-sample-presentation edit-song-sample :edit-sample
  "Edit this sample."
  ((object stream)
   (format stream "Edit ~A ~A    " (s-time object) (s-form object))))


(define-presentation-to-command-translator move-song-sample
    (song-sample com-move-song-sample wav-clim
		 :gesture :move-sample
		 :documentation "Move this sample"
		 :pointer-documentation ((object stream)
					 (format stream "Move ~A ~A   " (s-time object) (s-form object))))
    (object x y)
  (list object x y))

(define-presentation-to-command-translator copy-song-sample
    (song-sample com-copy-song-sample wav-clim
		 :gesture :copy-sample
		 :documentation "Copy this sample"
		 :pointer-documentation ((object stream)
					 (format stream "Copy ~A ~A   " (s-time object) (s-form object))))
    (object x y)
  (list object x y))


(define-song-sample-presentation delete-song-sample :delete-sample
  "Delete this sample."
  ((object stream)
   (format stream "Delete ~A ~A    " (s-time object) (s-form object))))


(define-song-sample-presentation edit-song-sample-time nil
  "Edit sample time")

(define-song-sample-presentation edit-song-sample-form nil
  "Edit sample form")

(define-song-sample-presentation edit-song-sample-pos nil
  "Edit sample position")

(define-song-sample-presentation edit-song-sample-tags nil
  "Edit sample tags")

(define-song-sample-presentation edit-song-sample-color nil
  "Edit sample color")

(define-song-sample-presentation edit-song-sample-length nil
  "Edit sample length")
