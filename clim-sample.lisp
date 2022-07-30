;;; CL-Wav-Synth -  Manipulate WAV files
;;;
;;; Copyright (C) 2006 Philippe Brochard (hocwp@free.fr)
;;;
;;; #Date#: Sun Sep  3 20:36:44 2006
;;;
;;; **********************************************
;;; The authors grant you the rights to distribute
;;; and use this software as governed by the terms
;;; of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html),
;;; known as the LLGPL.
;;; **********************************************


(in-package :cl-wav-synth-clim)


(defmethod convert-point-to-pane (x y (pane sample-pane))
  "Convert x, y to index, amplitude in pane coordinates"
  (let ((width (bounding-rectangle-width pane))
	(height (bounding-rectangle-height pane))
	(index (truncate (sample-index pane)))
	(len (truncate (sample-length pane)))
	(ampl (sample-ampl pane))
	(pos (sample-pos pane)))
    (values (truncate (+ (* x (/ (- len 1) width)) index))
	    (truncate (* (- (* height pos) y) (/ ampl height 0.5))))))

(defmethod convert-pane-to-point (index ampl (pane sample-pane))
  "Convert index, ampl to x, y in pane coordinates"
  (let ((width (bounding-rectangle-width pane))
	(height (bounding-rectangle-height pane))
	(pane-index (truncate (sample-index pane)))
	(len (truncate (sample-length pane)))
	(pane-ampl (sample-ampl pane))
	(pane-pos (sample-pos pane)))
    (values (* (- index pane-index) (/ width (- len 1)))
	    (- (* height pane-pos) (* ampl (/ height pane-ampl 2))))))



;;; Repaint methods
(defun bsubseq (seq &optional (start 0) (end nil))
  "Like as subseq but in [0-length seq]"
  (let* ((len (length seq))
	 (s (min (max 0 start)
		 len))
	 (e (max (min len
		      (or end len))
		 s)))
    (subseq seq s e)))


(defun draw-sample (frame pane)
  (labels ((display-time (x y val align-x align-y)
	     (draw-text* pane (format nil "~,2Fs (~D)"
				      (app-sample-to-time val)
				      (truncate val))
			 x y :align-x align-x :align-y align-y :ink +yellow+)
	     (force-output pane))
	   (display-ampl (x y val align-x align-y)
	     (draw-text* pane (format nil "~,2F (~D)"
				      (/ val (sample-max-ampl (sample frame)))
				      (truncate val))
			 x y :align-x align-x :align-y align-y :ink +yellow+)
	     (force-output pane))
	   (display-flags ()
	     (dolist (flag (flag-list pane))
	       (present flag 'sample-flag :stream pane)))
	   (display-channel-warning (x y align-x align-y)
	     (when (>= (sample-n-channels (sample frame)) 2)
	       (draw-text* pane (format nil "~A channels sample" (sample-n-channels (sample frame)))
			   x y :align-x align-x :align-y align-y :ink +red+))))
    (unless (sample frame)
      (setf (sample frame) (null-sample 0.001)))
    (let* ((width (bounding-rectangle-width pane))
	   (height (bounding-rectangle-height pane))
	   (zero-y (* height (sample-pos pane)))
	   (length (truncate (sample-length pane)))
	   (index (truncate (sample-index pane)))
	   (ampl (sample-ampl pane))
	   (array (bsubseq (data (sample frame)) index
			   (+ index length)))
	   (len (length array))
	   (dx (/ width (1- len)))
	   (idx (truncate (/ dx)))
	   (dy (/ height ampl 2))
	   (draw-vertbar (and (draw-vertbar *application-frame*)
			      (>= dx *sample-details*))))
      (draw-line* pane 0 zero-y width zero-y :ink +black+)
      (if (<= idx 1)
	  (loop for i below (1- len) do
		(draw-line* pane
			    (* i dx) (- zero-y (* dy (aref array i)))
			    (* (1+ i) dx) (- zero-y (* dy (aref array (1+ i))))
			    :ink +blue+)
		(when draw-vertbar
		  (draw-line* pane
			      (* i dx) zero-y
			      (* i dx) (- zero-y (* dy (aref array i)))
			      :ink +red+)))
	  (loop for i below width do
		(loop for j below idx
		      maximize (aref array (+ (* i idx) j)) into max
		      minimize (aref array (+ (* i idx) j)) into min
		      finally (draw-line* pane
					  i (- zero-y (* dy max))
					  i (- zero-y (* dy min))
					  :ink +blue+))))
      (display-time 0 height index :left :bottom)
      (display-time width height (+ index (sample-length pane)) :right :bottom)
      (display-ampl 0 0 ampl :left :top)
      (display-channel-warning (/ width 2) 0 :center :top)
      (display-flags))))



(defun draw-freqs (frame pane)
  (let* ((width (bounding-rectangle-width pane))
	 (height (bounding-rectangle-height pane))
	 (len (length (base-freqs frame)))
	 (dx (/ width (1+ len))))
    (draw-line* pane 0 0 width 0 :ink +black+)
    (draw-line* pane 0 (1- height) width (1- height) :ink +black+)
    (loop for freq in (base-freqs frame)
	  for i from 1 do
	  (draw-line* pane
		      (* i dx)  height
		      (* i dx)  (* height (- 1 (second freq))) :ink +red+)
	  (draw-text* pane (format nil "~,3F (~,1FdB)" (second freq)
				   (if (zerop (second freq))
				       -1
				       (* 20 (log (second freq)))))
		      (* i dx)  (* height (- 1 (second freq)))
		      :ink +white+ :align-x :center :align-y :top))))

(defun draw-phase (frame pane)
  (let* ((width (bounding-rectangle-width pane))
	 (height (bounding-rectangle-height pane))
	 (height/2 (/ height 2))
	 (len (length (base-freqs frame)))
	 (dx (/ width (1+ len))))
    (draw-line* pane 0 height/2 width height/2 :ink +black+)
    (loop for freq in (base-freqs frame)
	  for i from 1
	  do
	  (draw-line* pane
		      (* i dx)  height/2
		      (* i dx) (- height/2 (* height/2 (/ (third freq) *spi*)))
		      :ink +green+)
	  (draw-text* pane (format nil "~,1F" (* 180 (/ (third freq) *spi*)))
		      (* i dx) (- height/2 (* height/2 (/ (third freq) *spi*)))
		      :ink +white+ :align-x :center :align-y :top))))

;;; --- Repaint methods



;;; Handle Motion methods
(defun handle-point-freq-pane (pane x y)
  (let* ((frame (pane-frame pane))
	 (width (bounding-rectangle-width pane))
	 (height (bounding-rectangle-height pane))
	 (len (length (base-freqs frame)))
	 (dx (/ width (1+ len)))
	 (index (bound-value (truncate (- (/ x dx) 0.5)) 0 (1- len))))
    (setf (second (nth index (base-freqs frame)))
	  (float (- 1 (/ y height))))
    (setf (sample frame) (build-sample))
    (update-all-panes)))


(defun handle-point-phase-pane (pane x y)
  (let* ((frame (pane-frame pane))
	 (width (bounding-rectangle-width pane))
	 (height (bounding-rectangle-height pane))
	 (height/2 (/ height 2))
	 (len (length (base-freqs frame)))
	 (dx (/ width (1+ len)))
	 (index (bound-value (truncate (- (/ x dx) 0.5)) 0 (1- len))))
    (setf (third (nth index (base-freqs frame)))
	  (float (* (- 1 (/ y height/2)) *spi*)))
    (setf (sample frame) (build-sample))
    (update-all-panes)))



(defun handle-pane-motion (pane x1 y1 handle-point-fun)
  (funcall handle-point-fun pane x1 y1)
  (block processor
    (tracking-pointer
	(pane)
      (:pointer-motion (&key window x y)
		       (declare (ignore window))
		       (funcall handle-point-fun pane x y))
      (:pointer-button-release (&key event x y)
			       (when (= (pointer-event-button event)
					+pointer-left-button+)
				 (funcall handle-point-fun pane x y)
				 (return-from processor nil))))))


(define-presentation-to-command-translator freq-motion
    (blank-area com-freq-motion wav-clim
                :gesture :select :echo t
                :tester ((window) (typep window 'freq-pane)))
    (x y window)
  (list x y window))

(define-wav-clim-command (com-freq-motion :name t :menu nil)
    ((x 'real) (y 'real) (window 'freq-pane))
  (handle-pane-motion window x y #'handle-point-freq-pane))



(define-presentation-to-command-translator phase-motion
    (blank-area com-phase-motion wav-clim
                :gesture :select :echo t
                :tester ((window) (typep window 'phase-pane)))
    (x y window)
  (list x y window))

(define-wav-clim-command (com-phase-motion :name t :menu nil)
    ((x 'real) (y 'real) (window 'phase-pane))
  (handle-pane-motion window x y #'handle-point-phase-pane))



(define-presentation-to-command-translator sample-reset-bound-values
    (blank-area com-sample-reset-bound-values wav-clim
                :gesture nil
		:echo t
                :tester ((window) (typep window 'sample-pane)))
    (window)
  (list window))

(define-wav-clim-command (com-sample-reset-bound-values :name t :menu nil)
    ((window 'sample-pane))
  (declare (ignore window))
  (com-reset-bound-values))



(let ((ltext "Plop") (l-n-index 0) (l-n-ampl 0)
      (l-trunc-x 0) (l-trunc-y 0))
  (defun draw-sample-info-init ()
    (setf ltext nil
	  l-n-index nil l-n-ampl nil
	  l-trunc-x nil l-trunc-y nil))

  (labels ((draw-flag (window nx ny cv-x cv-y)
	     (draw-circle* window cv-x cv-y 1 :ink +flipping-ink+ :filled nil)
	     (draw-text* window (format nil "~A" nx)
			 cv-x (- cv-y 5) :align-x :center :align-y :bottom :ink +flipping-ink+)
	     (draw-text* window (format nil "~A" ny)
			 cv-x (+ cv-y 5) :align-x :center :align-y :top :ink +flipping-ink+)))
    (defun draw-sample-info-text (window x y width max-ampl)
      (multiple-value-bind (n-index n-ampl)
	  (convert-point-to-pane x y window)
	(multiple-value-bind (trunc-x trunc-y)
	    (convert-pane-to-point n-index n-ampl window)
	  (when ltext 
	    (draw-text* window ltext width 0
			:ink (pane-background window) :align-x :right :align-y :top))
	  (setf ltext (format nil "Time: ~,2F (~A)  Amplitude: ~,2F (~A)"
			      (float (app-sample-to-time n-index))
			      n-index
			      (/ n-ampl max-ampl)
			      n-ampl))
	  (draw-text* window ltext width 0
		      :ink +white+ :align-x :right :align-y :top)
	  (when (and l-n-index l-n-ampl l-trunc-x l-trunc-y)
	    (draw-flag window l-n-index l-n-ampl l-trunc-x l-trunc-y))
	  (setf l-n-index n-index l-n-ampl n-ampl
		l-trunc-x trunc-x l-trunc-y trunc-y)
	  (draw-flag window n-index n-ampl trunc-x trunc-y)
	  (force-output window))))

    (defun hide-sample-info-text (window)
      (when (and l-n-index l-n-ampl l-trunc-x l-trunc-y)
	(draw-flag window l-n-index l-n-ampl l-trunc-x l-trunc-y)))))


(define-wav-clim-command (com-find-sample-time-amplitude :name t :menu nil)
    ((x 'real) (y 'real) (window 'sample-pane))
  (let* ((width (bounding-rectangle-width window))
	 (max-ampl (sample-max-ampl (sample *application-frame*))))
    (draw-sample-info-init)
    (draw-sample-info-text window x y width max-ampl)
    (block processor
      (tracking-pointer (window)
	(:pointer-motion (&key x y)
			 (draw-sample-info-text window x y width max-ampl))
	(:pointer-button-release (&key x y)
				 (return-from processor (convert-point-to-pane x y window)))))))


;;; --- Handle motion methods







;;; Sample Zooming
(define-wav-clim-command (com-sample-center :name t :menu nil)
    ((x 'real) (y 'real) (map-sample 'sample-pane))
  (multiple-value-bind (index ampl)
      (com-find-sample-time-amplitude x y map-sample)
    (declare (ignore ampl))
    (with-slots (sample-index sample-length) map-sample
      (setf sample-index (bound-value (- index (/ sample-length 2))
				       0
				       (- (length (data (sample *application-frame*)))
					  sample-length 2))))))

(define-wav-clim-command (com-sample-time-zoom-in :name t :menu nil)
    ((x 'real) (y 'real) (map-sample 'sample-pane))
  (multiple-value-bind (index ampl)
      (com-find-sample-time-amplitude x y map-sample)
    (declare (ignore ampl))
    (with-slots (sample-index sample-length) map-sample
      (setf sample-length (max (/ sample-length 2) 2))
      (setf sample-index (bound-value (- index (/ sample-length 2))
				       0
				       (- (length (data (sample *application-frame*)))
					  sample-length 2))))))

(define-wav-clim-command (com-sample-time-zoom-out :name t :menu nil)
    ((x 'real) (y 'real) (map-sample 'sample-pane))
  (multiple-value-bind (index ampl)
      (com-find-sample-time-amplitude x y map-sample)
    (declare (ignore ampl))
    (with-slots (sample-index sample-length) map-sample
      (setf sample-length (min (* sample-length 2)
			       (- (length (data (sample *application-frame*))) 2)))
      (setf sample-index (bound-value (- index (/ sample-length 2))
				       0
				       (- (length (data (sample *application-frame*)))
					  sample-length 2))))))



(define-presentation-to-command-translator sample-center
    (blank-area com-sample-center wav-clim
                :gesture :center :echo t
		:documentation "Center"
		:pointer-documentation "Center"
                :tester ((window) (typep window 'sample-pane)))
    (x y window)
  (list x y window))

(define-presentation-to-command-translator sample-time-zoom-in
    (blank-area com-sample-time-zoom-in wav-clim
                :gesture :zoom-in :echo t
		:documentation "Time Zoom In"
		:pointer-documentation "Time Zoom In"
                :tester ((window) (typep window 'sample-pane)))
    (x y window)
  (list x y window))

(define-presentation-to-command-translator sample-time-zoom-out
    (blank-area com-sample-time-zoom-out wav-clim
                :gesture :zoom-out :echo t
		:documentation "Time Zoom Out"
		:pointer-documentation "Time Zoom Out"
                :tester ((window) (typep window 'sample-pane)))
    (x y window)
  (list x y window))


(define-presentation-to-command-translator sample-time-wheel-zoom-in
    (blank-area com-sample-time-zoom-in wav-clim
                :gesture :wheel-zoom-in :echo t
		:documentation "Time Zoom In"
		:pointer-documentation "Time Zoom In"
                :tester ((window) (typep window 'sample-pane)))
    (x y window)
  (list x y window))

(define-presentation-to-command-translator sample-time-wheel-zoom-out
    (blank-area com-sample-time-zoom-out wav-clim
                :gesture :wheel-zoom-out :echo t
		:documentation "Time Zoom Out"
		:pointer-documentation "Time Zoom Out"
                :tester ((window) (typep window 'sample-pane)))
    (x y window)
  (list x y window))







;;; Display table

(define-command-table display-command-table)

(add-menu-item-to-command-table 'display-command-table "Set" :divider nil)

(define-command (com-set-amplitude :name t :menu "Amplitude"
				   :command-table display-command-table)
    ((var 'number :prompt "Amplitude"))
  (with-application-frame (frame)
    (let ((map-sample (find-pane-named frame 'map-sample)))
      (with-slots (sample-ampl) map-sample
	(setf sample-ampl var)))))

(define-command (com-set-position :name t :menu "Position"
				  :command-table display-command-table)
    ((var 'number :prompt "Position"))
  (with-application-frame (frame)
    (let ((map-sample (find-pane-named frame 'map-sample)))
      (with-slots (sample-pos) map-sample
	(setf sample-pos (bound-value var 0 1))))))


(define-command (com-set-index :name t :menu "Index"
			       :command-table display-command-table)
    ((var 'number :prompt "Index"))
  (with-application-frame (frame)
    (let ((map-sample (find-pane-named frame 'map-sample)))
      (with-slots (sample-index) map-sample
	(setf sample-index var)))))


(define-command (com-set-begining :name t :menu "Begining"
				  :command-table display-command-table)
    ((var 'number :prompt "Begining time"))
  (with-application-frame (frame)
    (let ((map-sample (find-pane-named frame 'map-sample)))
      (with-slots (sample-index) map-sample
	(setf sample-index (time-to-sample (sample-n-samples-per-sec (sample *application-frame*))
					   var))))))


(define-command (com-set-length :name t :menu "Length"
				:command-table display-command-table)
    ((var 'number :prompt "Length"))
  (with-application-frame (frame)
    (let ((map-sample (find-pane-named frame 'map-sample)))
      (with-slots (sample-length) map-sample
	(setf sample-length var)))))


(define-command (com-set-duration :name t :menu "Duration"
				  :command-table display-command-table)
    ((var 'number :prompt "Duration"))
  (with-application-frame (frame)
    (let ((map-sample (find-pane-named frame 'map-sample)))
      (with-slots (sample-length) map-sample
	(setf sample-length (time-to-sample (sample-n-samples-per-sec (sample *application-frame*))
					    var))))))



(add-menu-item-to-command-table 'display-command-table "Reset" :divider nil)

(define-command (com-reset-bound-values :name t :menu "Bound values"
					:command-table display-command-table)
    ()
  (with-application-frame (frame)
    (set-bound-value frame)))

(define-command (com-set-all-to-zero :name t :menu "All to zero"
				     :command-table display-command-table)
    ()
  (with-application-frame (frame)
    (loop for freq in (base-freqs frame) do
	  (setf (second freq) 0
		(third freq) 0))
    (setf (sample frame) (build-sample))))

(add-menu-item-to-command-table 'display-command-table "Display" :divider nil)


(define-command (com-redisplay :name t :menu "Redisplay"
			       :command-table display-command-table)
    ()
  ())


(add-command-table-to-listener 'display-command-table)

;;; --- Display table



;;; Build sample command table

(define-command-table build-sample-command-table)

(define-command (com-set-sample-per-sec :name t :menu "Set sample per second"
					:command-table build-sample-command-table)
    ((sample 'number :prompt "Sample Per Second"))
  (with-application-frame (frame)
    (when (numberp sample)
      (setf (n-samples-per-sec frame) sample)
      (setf (sample frame) (build-sample))
      (set-bound-value frame))))


(define-command (com-set-bits-per-sample :name t :menu "Set bits per second"
					 :command-table build-sample-command-table)
    ((bits 'number :prompt "Bits Per Second"))
  (with-application-frame (frame)
    (when (numberp bits)
      (setf (n-bits-per-sample frame) bits)
      (setf (sample frame) (build-sample))
      (set-bound-value frame))))


(define-command (com-set-play-time :name t :menu "Set play time"
				   :command-table build-sample-command-table)
    ((time 'number :prompt "Play Time"))
  (with-application-frame (frame)
    (when (numberp time)
      (setf (play-time frame) time)
      (setf (sample frame) (build-sample))
      (set-bound-value frame))))


(define-command (com-set-build-time :name t :menu "Set build time"
				    :command-table build-sample-command-table)
    ((period 'number :prompt "Build Time"))
  (with-application-frame (frame)
    (when (numberp period)
      (setf (build-time frame)
	    (* period (/ (loop for freq in (base-freqs frame)
			       minimize (first freq)))))
      (setf (sample frame) (build-sample))
      (set-bound-value frame))))



(add-command-table-to-listener 'build-sample-command-table)


(make-command-table 'set-sample-per-sec-command-table
		    :errorp nil
		    :menu '(("8000" :command (com-set-sample-per-sec 8000))
			    ("11025" :command (com-set-sample-per-sec 11025))
			    ("16000" :command (com-set-sample-per-sec 16000))
			    ("22050" :command (com-set-sample-per-sec 22050))
			    ("44100" :command (com-set-sample-per-sec 44100))
			    ("48000" :command (com-set-sample-per-sec 48000))
			    ("96000" :command (com-set-sample-per-sec 96000))))


(make-command-table 'set-bits-per-sample-command-table
		    :errorp nil
		    :menu '(("8" :command (com-set-bits-per-sample 8))
			    ("16" :command (com-set-bits-per-sample 16))
			    ("32" :command (com-set-bits-per-sample 32))))

(make-command-table 'set-play-time-command-table
		    :errorp nil
		    :menu '(("0.5 sec" :command (com-set-play-time 0.5))
			    ("1 sec" :command (com-set-play-time 1))
			    ("2 sec" :command (com-set-play-time 2))
			    ("5 sec" :command (com-set-play-time 5))
			    ("10 sec" :command (com-set-play-time 10))))

(make-command-table 'set-build-time-command-table
		    :errorp nil
		    :menu '(("0.5 period" :command (com-set-build-time 0.5))
			    ("1 period" :command (com-set-build-time 1))
			    ("2 period" :command (com-set-build-time 2))
			    ("3 period" :command (com-set-build-time 3))
			    ("4 period" :command (com-set-build-time 4))
			    ("5 period" :command (com-set-build-time 5))
			    ("6 period" :command (com-set-build-time 6))
			    ("7 period" :command (com-set-build-time 7))
			    ("8 period" :command (com-set-build-time 8))
			    ("9 period" :command (com-set-build-time 9))
			    ("10 period" :command (com-set-build-time 10))))


(make-command-table 'build-sample-menu-command-table
		    :errorp nil
		    :menu '(("Undo/Redo" :menu sample-undo-redo-command-table)
			    ("Display"       :menu display-command-table)
			    ("Set samples per second" :menu set-sample-per-sec-command-table)
			    ("Set bits per sample" :menu set-bits-per-sample-command-table)
			    ("Set play time" :menu set-play-time-command-table)
			    ("Set build time" :menu set-build-time-command-table)
			    ("Flags" :menu sample-flag-command-table)
			    ("Envelope" :menu sample-envelope-command-table)))




;;; --- Build sample command table

