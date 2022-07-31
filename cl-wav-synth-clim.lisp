;;; CL-Wav-Synth -  Manipulate WAV files
;;;
;;; Copyright (C) 2006 Philippe Brochard (hocwp@free.fr)
;;;
;;; #Date#: Wed Jan 31 23:03:25 2007
;;;
;;; **********************************************
;;; The authors grant you the rights to distribute
;;; and use this software as governed by the terms
;;; of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html),
;;; known as the LLGPL.
;;; **********************************************



(in-package :cl-user)

(defpackage :cl-wav-synth-clim
  (:nicknames :wav-clim)
  (:use :clim :clim-lisp :cl-wav-synth :uni-shell :bezier)
  (:export :run :wav-clim :wav-clim-song))

(in-package :cl-wav-synth-clim)


(defparameter *spi* (float pi 1f0))

(defparameter *sample-details* 5
  "Number of pixels between each sample before drawing vertical bar")

(defparameter *song-time-grid-details* 15
  "Number of pixels  before drawing the grid")
(defparameter *song-pos-grid-details* 15
  "Number of pixels  before drawing the grid")

#+MIKMOD
(setf mikmod:*mikmod-control-function*
      (lambda ()
	(stream-read-char-no-hang *standard-input*)))


(defun bound-value (val min max)
  (max (min val max) min))

(defun build-sample (&optional time)
  (with-application-frame (frame)
    (build-from-freq (base-freqs frame)
		     :n-samples-per-sec (n-samples-per-sec frame)
		     :n-bits-per-sample (n-bits-per-sample frame)
		     :time (or time (build-time frame)))))


(defun set-bound-value (frame)
  (when (sample frame)
    (with-slots (sample-ampl sample-length sample-pos sample-index)
	(find-pane-named frame 'map-sample)
      (let ((maxampl (max (* 2 (loop for i across (data (sample frame))
				     maximize (abs i)))
			  10)))
	(setf sample-index 0)
	(setf sample-pos 0.5)
	(setf sample-length (- (length (data (sample frame))) 2))
	(setf sample-ampl (* maxampl 0.75))))))

(defun app-sample-to-time (index)
  "Convert index to time for the application frame sample"
  (sample-to-time (sample-n-samples-per-sec (sample *application-frame*))
		  (/ index (sample-n-channels (sample *application-frame*)))))

(defun app-time-to-sample (time)
  "Convert time to index for the application frame sample"
  (time-to-sample (sample-n-samples-per-sec (sample *application-frame*))
		  (* time (sample-n-channels (sample *application-frame*)))))


(defun find-free-number (l)     ; stolen from stumpwm
  "Return a number that is not in the list l."
  (let* ((nums (sort l #'<))
	 (new-num (loop for n from 0 to (or (car (last nums)) 0)
			for i in nums
			when (/= n i)
			do (return n))))
    (if new-num
	new-num
	;; there was no space between the numbers, so use the last + 1
	(if (car (last nums))
	    (1+ (car (last nums)))
	    0))))



(defclass sample-flag ()
  ((number :initarg :number :initform -1 :accessor flag-number)
   (index :initarg :index :initform -1 :accessor flag-index)
   (ampl :initarg :ampl :initform -1 :accessor flag-ampl)))


(defclass sample-pane (application-pane)
  ((sample-ampl :initarg :sample-ampl :initform 1000 :accessor sample-ampl)
   (sample-length :initarg :sample-length :initform 1000 :accessor sample-length)
   (sample-pos :initarg :sample-pos :initform 0.5 :accessor sample-pos)
   (sample-index :initarg :sample-index :initform 0 :accessor sample-index)
   (flag-list :initarg :flag-list :initform nil :accessor flag-list)
   (sample-filename :initarg :sample-filename :initform "test-sample.wav"
		    :accessor sample-filename)))

(defclass freq-pane (application-pane)
  ())

(defclass phase-pane (application-pane)
  ())

(defclass song-pane (application-pane)
  ((song-origine-x :initarg :song-origine-x :initform -1.5 :accessor song-origine-x)
   (song-origine-y :initarg :song-origine-y :initform -1 :accessor song-origine-y)
   (song-scaling :initarg :song-scaling :initform 100 :accessor song-scaling)
   (song-sample-radius :initarg :song-sample-radius :initform 0.1 :accessor song-sample-radius)
   (song-sample-filename :initarg :song-sample-filename :initform "test-song.song"
			 :accessor song-sample-filename)
   (song-grid-x :initarg :song-grid-x :initform 1 :accessor song-grid-x)
   (song-grid-y :initarg :song-grid-y :initform 1 :accessor song-grid-y)
   (song-grid-on? :initarg :song-grid-on? :initform t :accessor song-grid-on?)
   (song-snap-x :initarg :song-snap-x :initform 1 :accessor song-snap-x)
   (song-snap-y :initarg :song-snap-y :initform 1 :accessor song-snap-y)
   (song-snap-on? :initarg :song-snap-on? :initform t :accessor song-snap-on?)))

(defclass graphical-view (view)
  ())

(defparameter +graphical-view+ (make-instance 'graphical-view))



(define-application-frame wav-clim (clim-listener::listener)
  ((sample :initarg :sample :initform nil :accessor sample)
   (application :initarg :application :initform 'sample :accessor application)
   (base-freqs :initarg :base-freqs
	       :initform '((100 1 0) (200 0 0) (300 0 0) (400 0 0) (500 0 0))
	       :accessor base-freqs)
   (song :initarg :song :initform nil :accessor song)
   (draw-vertbar :initarg :draw-vertbar :initform t :accessor draw-vertbar)
   (build-time :initarg :build-time :initform (* 3 (/ 100)) :accessor build-time)
   (play-time :initarg :play-time :initform 1 :accessor play-time)
   (n-samples-per-sec :initarg :n-samples-per-sec :initform 8000
		      :accessor n-samples-per-sec)
   (n-bits-per-sample :initarg :n-bits-per-sample :initform 8
		      :accessor n-bits-per-sample))
  (:command-table (wav-clim
		   :inherit-from (clim-listener::application-commands
				  clim-listener::lisp-commands
				  clim-listener::filesystem-commands
				  clim-listener::show-commands
				  file-command-table
				  application-command-table
				  build-sample-menu-command-table
				  build-song-menu-command-table
				  listener-command-table
				  help-command-table)
		   :menu (("File"          :menu file-command-table)
			  ("Layout"        :menu application-command-table)
			  ("Sample"        :menu build-sample-menu-command-table)
			  ("Song"          :menu build-song-menu-command-table)
			  ("Listener"      :menu listener-command-table)
			  ("Help"          :menu help-command-table))))
  (:panes
   (map-sample (make-pane 'sample-pane :min-heigth 200
			  :scroll-bars nil
			  :foreground +white+ :background +grey+
			  :display-time :command-loop
			  :display-function #'draw-sample
			  :default-view +graphical-view+))
   (map-freq (make-pane 'freq-pane :min-heigth 100
			:scroll-bars nil
			:foreground +white+ :background +grey+
			:display-time :command-loop
			:display-function #'draw-freqs))
   (map-phase (make-pane 'phase-pane :min-heigth 100
			 :scroll-bars nil
			 :foreground +white+ :background +grey+
			 :display-time :command-loop
			 :display-function #'draw-phase))
   (map-song (make-pane 'song-pane :min-heigth 100 :scroll-bars nil
			:foreground +white+ :background +grey+
			:display-function #'draw-song
			:default-view +graphical-view+))
   (pointer-doc :pointer-documentation)
   (interactor-container (make-clim-stream-pane
			  :type 'clim-listener::listener-interactor-pane
			  :name 'interactor
			  :scroll-bars t
			  :default-view clim-listener::+listener-view+)))
  (:layouts
   (default
       (vertically ()
	 (1/2 map-sample)
	 (1/2 interactor-container)
	 pointer-doc))
   (view-sample
    (vertically ()
      (1/2 map-sample)
      (1/2 interactor-container)
      pointer-doc))
   (view-sample-1-3
    (vertically ()
      (1/3 map-sample)
      (2/3 interactor-container)
      pointer-doc))
   (view-sample-ni
    (vertically ()
      (9/10 map-sample)
      (1/10 interactor-container)
      pointer-doc))
   (build-sample
    (vertically ()
      (1/3 map-sample)
      (1/6 map-freq)
      (1/6 map-phase)
      (1/3 interactor-container)
      pointer-doc))
   (build-sample-left
    (vertically ()
      (horizontally ()
	(1/2 (vertically ()
	       (1/2 map-sample)
	       (1/4 map-freq)
	       (1/4 map-phase)))
	(1/2 interactor-container))
      pointer-doc))
   (build-sample-ni
    (vertically ()
      (9/20 map-sample)
      (5/20 map-freq)
      (5/20 map-phase)
      (1/20 interactor-container)))
   (build-song
    (vertically ()
      (1/2 map-song)
      (1/2 interactor-container)
      pointer-doc))
   (build-song-1-3
    (vertically ()
      (1/3 map-song)
      (2/3 interactor-container)
      pointer-doc))
   (build-song-2-3
    (vertically ()
      (2/3 map-song)
      (1/3 interactor-container)
      pointer-doc))
   (view-listener
    (vertically ()
      interactor-container
      pointer-doc)))
  (:top-level (default-frame-top-level :prompt 'clim-listener::print-listener-prompt)))




;;; Generic functions
(defgeneric convert-point-to-pane (x y pane))
(defgeneric convert-pane-to-point (x y pane))




(defmethod frame-standard-output ((frame wav-clim))
  (get-frame-pane frame 'interactor))


(defmethod adopt-frame :after (frame-manager (frame wav-clim))
  (declare (ignore frame-manager))
  (set-bound-value frame))

(defun update-all-panes ()
  (redisplay-frame-panes *application-frame*))



;;; CLim listener internal hack (not sure it's the right way, but it works :)
(make-command-table 'listener-command-table
		    :errorp nil
		    :menu '(("Application" :menu clim-listener::application-commands)
			    ("Lisp"        :menu clim-listener::lisp-commands)
			    ("Filesystem"  :menu clim-listener::filesystem-commands)
			    ("Show"        :menu clim-listener::show-commands)))

(defun add-command-table-to-listener (command-table)
  (map-over-command-table-names (lambda (name command)
				  (when (eql *package* (symbol-package command))
				    (add-command-to-command-table command
								  'clim-listener::application-commands
								  :name name :menu nil :errorp nil)))
				command-table))
;;; ---



;;; Gesture definitions
(define-gesture-name :center :pointer-button (:left))
(define-gesture-name :zoom-in :pointer-button (:left :meta))
(define-gesture-name :zoom-out :pointer-button (:right :meta))
(define-gesture-name :wheel-zoom-in :pointer-button (:wheel-up))
(define-gesture-name :wheel-zoom-out :pointer-button (:wheel-down))



(define-gesture-name :add-sample :pointer-button (:middle))

(define-gesture-name :edit-sample :pointer-button (:middle :control))
(define-gesture-name :move-sample :pointer-button (:left))
(define-gesture-name :copy-sample :pointer-button (:left :control))
(define-gesture-name :play-sample :pointer-button (:right :control))

(define-gesture-name :delete-sample :pointer-button (:middle :meta))

(define-gesture-name :move-selection :pointer-button (:left :control))
(define-gesture-name :delete-selection :pointer-button (:middle :control))
(define-gesture-name :copy-selection :pointer-button (:right :control))

(define-gesture-name :sample-draw :pointer-button (:left :control))
(define-gesture-name :sample-env :pointer-button (:right :control))

(define-gesture-name :sample-draw-bezier :pointer-button (:left :control :meta))
(define-gesture-name :sample-env-bezier :pointer-button (:right :control :meta))




;;; Startup methods
(defun run (&optional (app :sample))
  (let ((application 'sample)
	(layout 'default)
	(draw-vertbar t)
	(sample nil))
    (ecase app
      (:sample nil)
      (:freq (setf application 'build-sample
		   layout 'build-sample
		   draw-vertbar nil
		   sample (build-from-freq '((100 1) (300 0) (500 0))
					   :n-samples-per-sec 8000
					   :n-bits-per-sample 8
					   :time (* 3 (/ 440)))))
      (:song (setf application 'build-song
		   layout 'build-song))
      (:listener (setf layout 'view-listener)))
    (clim-listener::run-frame-top-level
     (make-application-frame 'wav-clim
			     :application application
			     :sample sample
			     :width 800 :height 600
			     :current-layout layout
			     :draw-vertbar draw-vertbar
			     :system-command-reader nil)
     :listener-funcall nil)))




(defgeneric wav-clim (object))

(defmethod wav-clim ((sample sample))
  (clim-listener::run-frame-top-level
   (make-application-frame 'wav-clim
			   :application 'sample
			   :sample sample
			   :width 800 :height 600
			   :current-layout 'view-sample
			   ;;:current-layout 'default
			   :system-command-reader nil)
   :listener-funcall nil))

(defmethod wav-clim ((spectre spectre))
  (clim-listener::run-frame-top-level
   (make-application-frame 'wav-clim
			   :application 'spectre
			   :sample spectre
			   :width 800 :height 600
			   :current-layout 'view-sample
			   :system-command-reader nil)))

(defmethod wav-clim ((base-freqs list))
  (clim-listener::run-frame-top-level
   (make-application-frame 'wav-clim
			   :application 'build-sample
			   :width 800 :height 600
			   :sample (build-from-freq base-freqs
						    :n-samples-per-sec 8000
						    :n-bits-per-sample 8
						    :time (* 3 (/ 440)))
			   :base-freqs base-freqs
			   :current-layout 'build-sample
			   :draw-vertbar nil
			   :system-command-reader nil)))

(defun wav-clim-song (song)
  (clim-listener::run-frame-top-level
   (make-application-frame 'wav-clim
			   :song (copy-list song)
			   :application 'build-song
			   :width 800 :height 600
			   :current-layout 'build-song
			   :system-command-reader nil)))
;;; --- Startup methods
  

;;; File command table
(define-command-table file-command-table)

(add-menu-item-to-command-table 'file-command-table "Filesystem"
				:menu 'clim-listener::filesystem-commands)

(add-menu-item-to-command-table 'file-command-table "File" :divider nil)

(define-command (com-play :name t :menu t
			  :command-table file-command-table)
    ()
  (with-application-frame (frame)
    (when (equal (application frame) 'build-sample)
      (setf (sample frame) (build-sample (play-time frame))))
    (case (application frame)
      ((sample build-sample) (play (sample frame)))
      (build-song (com-build-song)
		  (com-play-song)))
    (when (equal (application frame) 'build-sample)
      (setf (sample frame) (build-sample)))))


(add-menu-item-to-command-table 'file-command-table "Load as" :divider nil)

(define-command (com-load-as-sample :name t :menu "Sample"
				    :command-table file-command-table)
    ((path 'pathname :insert-default t
	   :default (sample-filename (find-pane-named *application-frame* 'map-sample))))
  (with-application-frame (frame)
    (let ((map-sample (find-pane-named frame 'map-sample)))
      (format t "Loading: ~A as sample~%" path)
      (setf (sample-filename map-sample) path)
      (ignore-errors
	(setf (sample frame) (read-sample path)))
      (set-bound-value frame))))

(define-command (com-load-as-spectrum :name t :menu "Spectrum"
				      :command-table file-command-table)
    ((path 'pathname)
     (freq 'number :prompt "Frequency"))
  (with-application-frame (frame)
    (format t "Loading: ~A as spectrum~%" path)
    (ignore-errors
      (setf (base-freqs frame) (read-freq-from-file path freq)))
    (setf (sample frame) (build-sample))))

(define-command (com-load-as-song :name t :menu "Song"
				  :command-table file-command-table)
    ((path 'pathname :insert-default t
	   :default (song-sample-filename (find-pane-named *application-frame* 'map-song))))
  (with-application-frame (frame)
    (let* ((map-song (find-pane-named frame 'map-song)))
      (if (probe-file path)
	  (progn
	    (setf (song-sample-filename map-song) path)
	    (format t "Loading ~A as song~%" path)
	    (read-song path)
	    (setf (song frame) *current-song*))
	  (format t "Error: can't open ~A~%" path)))))


(add-menu-item-to-command-table 'file-command-table "Save as" :divider nil)

(define-command (com-save-as-sample :name t :menu "Sample"
				    :command-table file-command-table)
    ((path 'pathname :insert-default t
	   :default (sample-filename (find-pane-named *application-frame* 'map-sample))))
  (with-application-frame (frame)
    (let ((map-sample (find-pane-named frame 'map-sample)))
      (format t "Saving: ~A as sample~%" path)
      (setf (sample-filename map-sample) path)
      (ignore-errors
	(write-sample path (sample frame))))))

(define-command (com-save-as-song :name t :menu "Song"
				  :command-table file-command-table)
    ((path 'pathname :insert-default t
	   :default (song-sample-filename (find-pane-named *application-frame* 'map-song))))
  (with-application-frame (frame)
    (let ((map-song (find-pane-named frame 'map-song)))
      (format t "Saving: ~A as song~%" path)
      (setf (song-sample-filename map-song) path)
      (ignore-errors
	(write-song path (song frame))))))

(add-menu-item-to-command-table 'file-command-table "Exec" :divider nil)

(define-command (com-exec-from-string :name t :menu "Exec"
				      :command-table file-command-table)
    ((command 'string :prompt "Exec"))
  (format t "Starting: ~A~%" command)
  (ignore-errors
    (ushell:ush command)))


(add-menu-item-to-command-table 'file-command-table "Player"
				:menu 'player-command-table)


(add-menu-item-to-command-table 'file-command-table "Quit" :divider nil)

(define-command (com-quit :name t :menu t
			  :command-table file-command-table
			  :keystroke (#\q :meta :control))
    ()
  (frame-exit *application-frame*))



(add-command-table-to-listener 'file-command-table)

;; Player command table
(define-command-table player-command-table)

(define-command (com-info-on-player :name t :menu t
				    :command-table player-command-table)
    ()
  (player-info))

(add-menu-item-to-command-table 'player-command-table "Set" :divider nil)

(defmacro define-com-set-player (com-name menu cmdname)
  `(define-command (,com-name :name t :menu ,menu
		    :command-table player-command-table)
    ()
    (format t "Setting ~A as the default player" ,cmdname)
    (setf cl-wav-synth::*default-player* ,cmdname)))

(define-com-set-player com-set-player-default-player "Default player" 'dplay)
#+MIKMOD
(define-com-set-player com-set-player-mikmod-player "Mikmod internal player" 'mikmod-play)
(define-com-set-player com-set-player-sox-player "Sox player" 'sox-play)
(define-com-set-player com-set-player-xmms "XMMS" 'xmms)
(define-com-set-player com-set-player-snd "SND" 'snd)
(define-com-set-player com-set-player-audacity "Audacity" 'audacity)
(define-com-set-player com-set-player-cool-player "Cool Player" 'cool-player)
(define-com-set-player com-set-player-totem "Totem" 'totem)
(define-com-set-player com-set-player-bmp "Beep Media Player" 'bmp)
(define-com-set-player com-set-player-macplay "MacOS Player" 'macplay)

(define-command (com-set-player-other :name t :menu "Other player"
				      :command-table player-command-table)
    ((command 'string :prompt "Player command line"))
  (format t "Setting ~A as the default player" command)
  (setf cl-wav-synth::*default-player* command))




(add-command-table-to-listener 'player-command-table)
;;; ---  File command table






;;; Layout commands

(define-command-table application-command-table)

(add-menu-item-to-command-table 'application-command-table "View sample" :divider nil)

(define-command (com-set-layout-view-sample :name t :menu "Normal"
					    :command-table application-command-table)
    ()
  (with-application-frame (frame)
    (setf (application frame) 'sample)
    (setf (draw-vertbar frame) t)
    (setf (frame-current-layout frame) 'view-sample)))

(define-command (com-set-layout-view-sample-1-3
		 :name t :menu "Normal 1/3 2/3"
		 :command-table application-command-table)
    ()
  (with-application-frame (frame)
    (setf (application frame) 'sample)
    (setf (draw-vertbar frame) t)
    (setf (frame-current-layout frame) 'view-sample-1-3)))

(define-command (com-set-layout-view-sample-no-listener
		 :name t :menu "No listener"
		 :command-table application-command-table)
    ()
  (with-application-frame (frame)
    (setf (application frame) 'sample)
    (setf (draw-vertbar frame) t)
    (setf (frame-current-layout frame) 'view-sample-ni)))


(add-menu-item-to-command-table 'application-command-table "Build sample" :divider nil)

(define-command (com-set-layout-build-sample :name t :menu "Normal"
					     :command-table application-command-table)
    ()
  (with-application-frame (frame)
    (setf (application frame) 'build-sample)
    (setf (sample frame) (build-sample))
    (setf (draw-vertbar frame) nil)
    (setf (frame-current-layout frame) 'build-sample)))



(define-command (com-set-layout-build-sample-left
		 :name t :menu "Normal (listener on left)"
		 :command-table application-command-table)
    ()
  (with-application-frame (frame)
    (setf (application frame) 'build-sample)
    (setf (sample frame) (build-sample))
    (setf (draw-vertbar frame) nil)
    (setf (frame-current-layout frame) 'build-sample-left)))


(define-command (com-set-layout-build-sample-no-listener
		 :name t :menu "No listener"
		 :command-table application-command-table)
    ()
  (with-application-frame (frame)
    (setf (application frame) 'build-sample)
    (setf (sample frame) (build-sample))
    (setf (draw-vertbar frame) nil)
    (setf (frame-current-layout frame) 'build-sample-ni)))

(add-menu-item-to-command-table 'application-command-table "Build song" :divider nil)

(define-command (com-set-layout-build-song
		 :name t :menu "Normal"
		 :command-table application-command-table)
    ()
  (with-application-frame (frame)
    (setf (application frame) 'build-song)
    (setf (frame-current-layout frame) 'build-song)))

(define-command (com-set-layout-build-song-1-3
		 :name t :menu "Normal 1/3 2/3"
		 :command-table application-command-table)
    ()
  (with-application-frame (frame)
    (setf (application frame) 'build-song)
    (setf (frame-current-layout frame) 'build-song-1-3)))

(define-command (com-set-layout-build-song-2-3
		 :name t :menu "Normal 2/3 1/3"
		 :command-table application-command-table)
    ()
  (with-application-frame (frame)
    (setf (application frame) 'build-song)
    (setf (frame-current-layout frame) 'build-song-2-3)))


(add-menu-item-to-command-table 'application-command-table "Listener" :divider nil)

(define-command (com-set-layout-view-listener
		 :name t :menu "Listener"
		 :command-table application-command-table)
    ()
  (with-application-frame (frame)
    (setf (frame-current-layout frame) 'view-listener)))


(add-command-table-to-listener 'application-command-table)

;;; --- Layout commands


;;; Help command table

(define-command-table help-command-table)


(define-command (com-short-help :name t :menu "Help"
				:command-table help-command-table)
    ()
  (with-application-frame (frame)
    (let ((stream (find-pane-named frame 'interactor)))
      (cl-wav-synth::internal-help 'help stream)
      (format stream "~&Wav defined functions are: ")
      (formatting-table (stream :multiple-columns t)
	(maphash #'(lambda (k v)
		     (declare (ignore v))
		     (formatting-row (stream)
		       (formatting-cell (stream)
			 (present (format nil "~A " k)) 'string :stream stream)))
		 cl-wav-synth::*wav-function-hash*)
	(format stream "~&")
	(force-output stream))
      nil)))


(define-command (com-show-help :name t :menu "Show help"
			       :command-table help-command-table)
    ((name 'string :prompt "Name:"))
  (cl-wav-synth::internal-help (intern (string-trim " " name) :cl-wav-synth)))

(define-command (com-long-help :name t :menu "Long help"
			       :command-table help-command-table)
    ()
  (long-help))

(define-presentation-to-command-translator com-show-help
    (string com-show-help wav-clim
	    :gesture :select
	    :documentation "Show help"
	    :pointer-documentation ((object stream)
				    (format stream "Show ~A  " object)))
    (object)
  (list object))



(add-menu-item-to-command-table 'help-command-table "Test" :divider nil)

(define-command (com-list-all-test :name t :menu "List all tests"
				   :command-table help-command-table)
    ()
  (list-all-test))

(define-command (com-run-test :name t :menu "Run a test"
			      :command-table help-command-table)
    ((num 'number :prompt "Test number"))
  (let ((name (intern (format nil "TEST~A" num))))
    (when (fboundp name)
      (funcall name))))

(define-command (com-print-test :name t :menu "View code test"
				:command-table help-command-table)
    ((num 'number :prompt "Test number"))
  (print-test num))


(add-command-table-to-listener 'help-command-table)




;;; CLIM Listener hack
(in-package :clim-listener)

;;; Grab from Clim Listener - Add: Ignore errors in eval
(fmakunbound 'com-eval)

(define-command (com-eval :menu t :command-table lisp-commands)
    ((form 'clim:form :prompt "form"))
  (let* ((- form)
         (values (multiple-value-list
		  (handler-case (eval form)
		   (error (condition) (format nil "~A" condition))))))
    (fresh-line)
    (shuffle-specials form values)
    (display-evalues values)
    (fresh-line)))



;; Test: editing form... (not sure it's the right way, but it works :)
(define-command (com-edit-form :name t :menu t :command-table lisp-commands)
    ((form 'expression :prompt "Form"))
  (multiple-value-bind (x1 y1)
      (stream-cursor-position *standard-input*)
    (let ((new-form (accept 'expression :stream (frame-standard-input *application-frame*)
			    :prompt "New form" :default form
			    :insert-default t)))
      (multiple-value-bind (x2 y2)
	  (stream-cursor-position *standard-input*)
	(declare (ignore x2))
	(let* ((interactor (find-pane-named *application-frame* 'cl-wav-synth-clim::interactor))
	       (output-history (stream-output-history interactor))
	       (width (bounding-rectangle-width interactor)))
	  (decf y1 (stream-line-height interactor))
	  (map-over-output-records-overlapping-region
   	     #'(lambda (rec) (delete-output-record rec output-history))
	     output-history (make-rectangle* 0 y1 width y2))
	  (draw-rectangle* interactor 0 y1 width y2 :ink (pane-background interactor))
	  (setf (stream-cursor-position *standard-input*) (values x1 y1))
	  (print-listener-prompt interactor *application-frame*)
	  (present new-form 'expression)
	  (fresh-line)
	  (force-output)
	  (execute-frame-command *application-frame* `(com-eval ,new-form)))))))


(define-presentation-to-command-translator edit-form
    (clim:command com-edit-form lisp-commands
		  :gesture :describe :echo t
		  :documentation ((object stream)
				  (format stream "Edit ~A" (second object)))
		  :pointer-documentation ((object stream)
					  (format stream "Edit ~A" (second object)))
		  :tester ((object) (eql (car object) 'com-eval)))
    (object)
  (list (second object)))


(define-presentation-to-command-translator edit-form-form
    (expression com-edit-form lisp-commands
		:gesture :describe :echo t
		:documentation ((object stream)
				(format stream "Edit ~A" object))
		:pointer-documentation ((object stream)
					(format stream "Edit ~A" object)))
    (object)
  (list object))


(define-presentation-to-command-translator eval-form
    (expression com-eval lisp-commands
		:gesture :select :echo t
		:documentation ((object stream)
				(format stream "Eval ~A" object))
		:pointer-documentation ((object stream)
					(format stream "Eval ~A" object)))
    (object)
  (list object))

(define-presentation-to-command-translator show-command-history
    (blank-area com-show-command-history lisp-commands
		:gesture :menu :echo t)
    ()
  ())




(define-command (com-ls :name t :menu nil :command-table lisp-commands)
    ((dir 'pathname :prompt "Directory"))
  (com-show-directory dir))



(define-mime-type (audio cl-wav)
    (:extensions "wav"))

(defmethod mime-type-to-command ((mime-type audio/cl-wav) pathname)
  (values `(wav-clim::com-load-as-sample ,pathname)
          (format nil "Load as sample ~A" (file-namestring pathname))
          (format nil "Load as sample ~A" pathname)))



(define-mime-type (text spectrum)
    (:extensions "spect"))

(defmethod mime-type-to-command ((mime-type text/spectrum) pathname)
  (values `(wav-clim::com-load-as-spectrum ,pathname 100)
          (format nil "Load as spectrum ~A" (file-namestring pathname))
          (format nil "Load as spectrum ~A" pathname)))


(define-mime-type (text song)
    (:extensions "song"))

(defmethod mime-type-to-command ((mime-type text/song) pathname)
  (values `(wav-clim::com-load-as-song ,pathname)
          (format nil "Load as song ~A" (file-namestring pathname))
          (format nil "Load as song ~A" pathname)))
