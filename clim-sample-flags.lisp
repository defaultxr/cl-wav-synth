;;; CL-Wav-Synth -  Manipulate WAV files
;;;
;;; Copyright (C) 2006 Philippe Brochard (hocwp@free.fr)
;;;
;;; #Date#: Sat Aug 26 13:40:54 2006
;;;
;;; **********************************************
;;; The authors grant you the rights to distribute
;;; and use this software as governed by the terms
;;; of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html),
;;; known as the LLGPL.
;;; **********************************************


(in-package :cl-wav-synth-clim)



(defun flag-time (flag)
  (app-sample-to-time (flag-index flag)))



;;; Flag presentation

(define-presentation-method present (sample-flag (type sample-flag) stream
						 (view textual-view) &key)
  (format stream "~A: ~,2Fs, ~A" (flag-number sample-flag)
	  (flag-time sample-flag)
	  (flag-ampl sample-flag)))

(define-presentation-method present (sample-flag (type sample-flag) stream
						 (view graphical-view) &key)
  (let* ((width (bounding-rectangle-width stream))
	 (height (bounding-rectangle-height stream)))
    (multiple-value-bind (x y)
	(convert-pane-to-point (flag-index sample-flag) (flag-ampl sample-flag) stream)
      (when (and (<= 0 x width) (<= 0 y height))
	(draw-circle* stream x y 1 :ink +yellow+)
	(draw-text* stream (format nil "~A" (flag-number sample-flag))
		    x (- y 5) :align-x :center :align-y :bottom :ink +yellow+)
	(draw-text* stream (format nil "~,2Fs"
				   (flag-time sample-flag))
		    x (+ y 5) :align-x :center :align-y :top :ink +yellow+)))))

;;; accept method for a sample-flag presentation (in any view mode):
(define-presentation-method accept ((type sample-flag) stream view &key)
  (declare (ignore view))
  (values
   (completing-from-suggestions (Stream :partial-completers '(#\Space))
     (let ((sample-map (find-pane-named *application-frame* 'sample-map)))
       (dolist (flag (flag-list sample-map))
	 (suggest (format nil "~A: ~,2Fs, ~A" (flag-number flag)
			  (flag-time flag)
			  (flag-ampl flag))
		  flag))))))


;;; Flag command table
(define-command-table sample-flag-command-table)

(add-menu-item-to-command-table 'sample-flag-command-table "Add" :divider nil)

(define-command (com-add-sample-flag :name t :menu "Add sample flag"
				     :command-table sample-flag-command-table)
    ((number 'number :prompt "Number")
     (index 'number :prompt "Index")
     (ampl 'number :prompt "Amplitude"))
  (with-application-frame (frame)
    (let* ((map-sample (find-pane-named frame 'map-sample))
	   (new-flag (make-instance 'sample-flag :number number :index (truncate index) :ampl ampl)))
      (push new-flag (flag-list map-sample)))))


(defun find-free-flag-number ()
  (with-application-frame (frame)
    (let ((map-sample (find-pane-named frame 'map-sample)))
      (find-free-number (mapcar #'(lambda (x) (flag-number x))  (flag-list map-sample))))))
      

(define-wav-clim-command (com-sample-add-sample-flag :name t :menu nil)
    ((x 'real) (y 'real) (window 'sample-pane))
  (multiple-value-bind (index ampl)
      (com-find-sample-time-amplitude x y window)
    (com-add-sample-flag (find-free-flag-number) index ampl)))


(define-presentation-to-command-translator sample-add-sample-flag
    (blank-area com-sample-add-sample-flag wav-clim
                :gesture :add-sample :echo t
		:documentation "Add Sample Flag"
		:pointer-documentation "Add Sample Flag"
                :tester ((window) (typep window 'sample-pane)))
    (x y window)
  (list x y window))


(define-command (com-move-sample-flag :name t :menu nil
				      :command-table sample-flag-command-table)
    ((sample-flag 'sample-flag)
     (x 'real) (y 'real))
  (with-application-frame (frame)
    (multiple-value-bind (index pos)
	(com-find-sample-time-amplitude x y (find-pane-named frame 'map-sample))
      (setf (flag-index sample-flag) (truncate index)
	    (flag-ampl sample-flag) pos))))



(define-presentation-to-command-translator move-sample-flag
    (sample-flag com-move-sample-flag wav-clim
		 :gesture :move-sample
		 :documentation "Move this flag"
		 :pointer-documentation ((object stream)
					 (format stream "Move flag ~A" (flag-number object))))
    (object x y)
  (list object x y))



(add-menu-item-to-command-table 'sample-flag-command-table "Delete" :divider nil)


(define-command (com-delete-sample-flag :name t :menu t
					:command-table sample-flag-command-table)
    ((sample-flag 'sample-flag))
  (with-application-frame (frame)
    (let ((map-sample (find-pane-named frame 'map-sample)))
      (setf (flag-list map-sample) (remove sample-flag (flag-list map-sample))))))

(define-presentation-to-command-translator delete-sample-flag
    (sample-flag com-delete-sample-flag wav-clim
		 :gesture :delete-sample
		 :documentation "Delete this flag"
		 :pointer-documentation ((object stream)
					 (format stream "Delete flag ~A" (flag-number object))))
    (object)
  (list object))


(define-command (com-delete-all-sample-flags :name t :menu t
					     :command-table sample-flag-command-table)
    ()
  (with-application-frame (frame)
    (let ((map-sample (find-pane-named frame 'map-sample)))
      (setf (flag-list map-sample) nil))))





(add-menu-item-to-command-table 'sample-flag-command-table "Set" :divider nil)

(define-command (com-set-sample-flag-index :name t :menu t
					   :command-table sample-flag-command-table)
    ((sample-flag 'sample-flag :prompt "Flag")
     (index 'number :prompt "Index"))
  (setf (flag-index sample-flag) (truncate index)))

(define-command (com-set-sample-flag-time :name t :menu t
					  :command-table sample-flag-command-table)
    ((sample-flag 'sample-flag :prompt "Flag")
     (time 'number :prompt "Time"))
  (setf (flag-index sample-flag) (truncate (app-time-to-sample time))))

(define-command (com-set-sample-flag-ampl :name t :menu t
					  :command-table sample-flag-command-table)
    ((sample-flag 'sample-flag :prompt "Flag")
     (ampl 'number :prompt "Amplitude"))
  (setf (flag-ampl sample-flag) (truncate ampl)))


(add-command-table-to-listener 'sample-flag-command-table)
