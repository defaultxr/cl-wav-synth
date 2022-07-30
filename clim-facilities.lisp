;;; CL-Wav-Synth -  Manipulate WAV files
;;;
;;; Copyright (C) 2006 Philippe Brochard (hocwp@free.fr)
;;;
;;; #Date#: Wed Jan 31 22:46:14 2007
;;;
;;; **********************************************
;;; The authors grant you the rights to distribute
;;; and use this software as governed by the terms
;;; of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html),
;;; known as the LLGPL.
;;; **********************************************

(in-package :cl-wav-synth)

(defun all-members (item-list list)
  "Return true if all items in item-list are in list"
  (not (set-difference item-list list)))



;;; Help override

(fmakunbound 'help)

(define-wav-function help (&optional name (stream *standard-output*))
  "Get the default help or a help on function
  Use (help) or (help 'a_function_name) for more details"
  (if (equal stream *standard-output*)
      (if name
	  (cl-wav-synth-clim::com-show-help name)
	  (cl-wav-synth-clim::com-short-help))
      (internal-help name stream)))


;;; Undo/Redo general system

(defparameter *undo-level* 0)
(defparameter *song-undo-level* 0)


(defun undo-name (level type)
  (format nil "~A-undo-~A.~A" type level type))


(defmacro general-undo (write-form set-form level new-level type)
  "Undo the effect of set-sample"
  `(progn
    ,write-form
    (setf ,level (or ,new-level (max (1- ,level) 0)))
    (let ((undo-name (undo-name ,level ,type)))
      (when (probe-file undo-name)
	,set-form))
    (format t "Level: ~A~%" ,level)
    (force-output)
    ,level))

(defmacro general-redo (set-form level new-level type)
  "Redo the effect of set-sample"
  `(progn
    (let ((redo-name (undo-name (or ,new-level (1+ ,level)) ,type)))
      (when (probe-file redo-name)
	,set-form
	(setf ,level (or ,new-level (1+ ,level)))))
    (format t "Level: ~A~%" ,level)
    (force-output)
    ,level))

(defmacro generel-clear-undo (level type max-level)
  "Clear all undo/redo saved states"
  `(progn
    (dotimes (i ,max-level)
      (let ((undo-name (undo-name i ,type)))
	(if (probe-file undo-name)
	    (delete-file undo-name)
	    (return))))
    (setf ,level 0)
    (format t "Level: ~A~%" ,level)
    (force-output)
    ,level))


(defun general-list-undo (level type max-level)
  "List all undo/redo saved states"
  (let ((cpt -1))
    (dotimes (i max-level)
      (let ((undo-name (undo-name i type)))
	(when (probe-file undo-name)
	  (incf cpt))))
    (format t "Level: ~A / ~A~%" level cpt)
    (force-output)
    level))






;;; Song facilities

(defvar *song-undo-redo-enabled?* t)

(defun save-song-undo-redo (song)
  (when *song-undo-redo-enabled?*
    (write-song (undo-name *song-undo-level* "song") song)
    (incf *song-undo-level*)))


;;(define-wav-function s-find (&key time form pos tags color length all)
;;  "Return a list of all song sample matching time/form/pos/tags/color/length
;;Check members in tags if it's a list"
;;  (loop for x in (cl-wav-synth-clim::song clim::*application-frame*)
;;	when (or all
;;		 (and (if time (= (s-time x) time) t)
;;		      (if form (equal (s-form x) form) t)
;;		      (if pos (= (s-pos x) pos) t)
;;		      (if tags
;;			  (if (consp (s-tags x))
;;			      (if (consp tags)
;;				  (all-members tags (s-tags x))
;;				  (member tags (s-tags x)))
;;			      (equal (s-tags x) tags))
;;			  t)
;;		      (if color (= (s-color x) color) t)
;;		      (if length (= (s-length x) length) t)))
;;	collect x))


(define-wav-macro s-find (&key time form pos tags color length all)
  "Return a list of all song samples matching time/form/pos/tags/color/length.
time, pos, color and length can be real numbers or conditions.
For examples:
  :time (= time 10) is the same as :time 10
  :pos (< 1 pos 2) find all samples with a position between 1 and 2
    and is the same as :pos (and (< 1 pos) (< pos 2))
Tags can be a symbol, a number or a list (or something else)
If tags is a list check for its members
For examples:
  :tags sel find all tags with sel in it.
     ex: (bar sel foo) and sel match
  :tags (sel bar) find all tags with sel and bar in it
     ex: (bar sel foo) match"
  (labels ((check-number (key symbol)
	     (if key
		 (if (numberp key)
		     `(= ,symbol ,key)
		     key)
		 t))
	   (check-tags (ktags)
	     (if ktags
		 `(if (consp tags)
		   ,(if (consp ktags)
			`(all-members ',ktags tags)
			`(member ',ktags tags))
		   (equal tags ',ktags))
		 t)))
    `(loop for x in (cl-wav-synth-clim::song clim::*application-frame*)
      when (with-slots (time form pos tags color length) x
	     (or ,all
		 (and ,(check-number time 'time)
		      ,(if form `(equal form ,form) t)
		      ,(check-number pos 'pos)
		      ,(check-tags tags)
		      ,(check-number color 'color)
		      ,(check-number length 'length))))
      collect x)))



;;(defparameter my-list '(1 2 3 4 10 20 30 40))
;;;;
;;;;;;(defmacro my-find (key)
;;;;;;  `(loop for x in my-list
;;;;;;    when (if (numberp ,key) (= ,key x) ,key)
;;;;;;    collect x))
;;;;
;;(defmacro my-find (key)
;;  `(loop for x in my-list
;;    when ,(if (numberp key) `(= ,key x) key)
;;    collect x)))
;;
;;(my-find 10) -> (10)  (my-find (< 1 x 10)) -> (2 3 4)
;;
;;(defun my-find-fun (key)  ; does not work
;;  (my-find key))
;;
;;(defmacro my-find-mac (key)
;;  `(my-find ,key))
;;
;;(my-find-mac 10) -> (10)  (my-find-mac (< 1 x 10)) -> (2 3 4)




(define-wav-function s-add (&key (time 0) form (pos 0) tags (color #x00FF00) (length 0.2))
  "Add a sample on song"
  (save-song-undo-redo (cl-wav-synth-clim::song clim::*application-frame*))
  (let ((new-sample (make-instance 'song-sample :time time
				   :form form :pos pos :tags tags :color color :length length)))
    (eval-song-sample-form new-sample)
    (push new-sample (cl-wav-synth-clim::song clim::*application-frame*))
    new-sample))


(define-wav-function s-copy (song-sample &key time form pos tags color length)
  "Copy a sample in a song"
  (save-song-undo-redo (cl-wav-synth-clim::song clim::*application-frame*))
  (s-add :time (or time (s-time song-sample))
	 :form (or form (s-form song-sample))
	 :pos (or pos (s-pos song-sample))
	 :tags (or tags (s-tags song-sample))
	 :color (or color (s-color song-sample))
	 :length (or length (s-length song-sample))))


(define-wav-macro s-delete (&key time form pos tags color length all)
  "Delete samples matching time/form/pos/tags/color/length as in s-find"
  `(progn
    (save-song-undo-redo (cl-wav-synth-clim::song clim::*application-frame*))
    (setf (cl-wav-synth-clim::song clim::*application-frame*)
     (set-difference (cl-wav-synth-clim::song clim::*application-frame*)
      (s-find :time ,time :form ,form :pos ,pos :tags ,tags
	      :color ,color :length ,length :all ,all)))
    nil))



(define-wav-macro s-map-fun (&key time form pos tags color length all fun)
  "Map a function over a sample in a song matching time/form/pos/tags/color/length
as in s-find. When 'all' is true, map on all samples in the song"
  `(progn
    (save-song-undo-redo (cl-wav-synth-clim::song clim::*application-frame*))
    (setf *song-undo-redo-enabled?* nil)
    (unwind-protect
	 (mapc ,fun (s-find :time ,time :form ,form :pos ,pos :tags ,tags
			    :color ,color :length ,length :all ,all))
      (setf *song-undo-redo-enabled?* t))
    nil))


(define-wav-macro s-map ((&key time form pos tags color length all) &body body)
  "Apply body on sample matching time/form/pos/tags/color/length as in s-find.
   Capture intentionally the sample into the variable 'it'
   and sample slots (as time, form, pos and tags) are bounds.
   When 'all' is true, apply on all samples in the song"
  `(s-map-fun :time ,time :form ,form :pos ,pos :tags ,tags :color ,color
    :length ,length :all ,all
    :fun (lambda (it)
	   (with-slots (time form pos tags color length) it
	     ,@body))))


(define-wav-function s-print (song-sample)
  "Print a sample in a song"
  (cl-wav-synth-clim::present song-sample)
  (terpri))

(define-wav-macro with-song (&body body)
  "Apply body on the current loaded song. Capture intentionally the song
  in the variable 'it'. (Note: a song is just a list of song-sample)"
  `(symbol-macrolet ((it (cl-wav-synth-clim::song cl-wav-synth-clim::*application-frame*)))
    (save-song-undo-redo it)
    ,@body))

(define-wav-macro set-song (&body body)
  "Set the current loaded song with the result of body.
  Capture intentionally the song in the variable 'it'.
  (Note: a song is just a list of song-sample)"
  `(symbol-macrolet ((it (cl-wav-synth-clim::song cl-wav-synth-clim::*application-frame*)))
    (save-song-undo-redo it)
    (setf it ,@body)
    t))


(define-wav-macro build-song-from-match (filename &key time form pos tags color length all)
  "Build a song in filename with samples in a song matching time/form/pos/tags/color/length
as in s-find.
   When 'all' is true, build from all samples in the song"
  `(progn
    (format t "Building in ~S~%" ,filename)
    (build-song ,filename (s-find :time ,time :form ,form :pos ,pos :tags ,tags
			   :color ,color :length ,length :all ,all))
    nil))



(define-wav-function s-save-undo ()
  "Save the current song form undo/redo operation"
  (save-song-undo-redo (cl-wav-synth-clim::song clim::*application-frame*)))


(define-wav-function s-undo (&optional (level nil))
  "Undo the effect of last song operation"
  (general-undo (write-song (undo-name *song-undo-level* "song")
			    (cl-wav-synth-clim::song cl-wav-synth-clim::*application-frame*))
		(setf (cl-wav-synth-clim::song cl-wav-synth-clim::*application-frame*)
		      (read-song undo-name))
		*song-undo-level* level "song"))
		


(define-wav-function s-redo (&optional (level nil))
  "Redo the effect of last song operation"
  (general-redo (setf (cl-wav-synth-clim::song cl-wav-synth-clim::*application-frame*)
		      (read-song redo-name))
		*song-undo-level* level "song"))


(define-wav-function s-clear-undo (&optional (max-level 100))
  "Clear all undo/redo saved states for song"
  (generel-clear-undo *song-undo-level* "song" max-level))


(define-wav-function s-list-undo (&optional (max-level 100))
  "List all undo/redo saved states for song"
  (general-list-undo *song-undo-level* "song" max-level))




;;; Sample facilities

(defun save-undo-redo (sample)
  (write-sample (undo-name *undo-level* "wav") sample)
  (incf *undo-level*))

(define-wav-macro with-sample (&body body)
  "Apply body on the current loaded sample. Capture intentionally the sample
  into the variable 'it'"
  `(symbol-macrolet ((it (cl-wav-synth-clim::sample cl-wav-synth-clim::*application-frame*)))
    (save-undo-redo it)
    ,@body))


(define-wav-macro set-sample (&body body)
  "Set the current loaded sample with the result of body.
   Capture intentionally the sample into the variable 'it'.
   And save the result with a undo/redo method"
  `(symbol-macrolet ((it (cl-wav-synth-clim::sample cl-wav-synth-clim::*application-frame*)))
    (save-undo-redo it)
    (setf it ,@body)))



(define-wav-function save-undo ()
  "Save the current sample form undo/redo operation"
  (save-undo-redo (cl-wav-synth-clim::sample cl-wav-synth-clim::*application-frame*)))

(define-wav-function undo (&optional (level nil))
    "Undo the effect of set-sample"
  (general-undo (write-sample (undo-name *undo-level* "wav")
			      (cl-wav-synth-clim::sample cl-wav-synth-clim::*application-frame*))
		(setf (cl-wav-synth-clim::sample cl-wav-synth-clim::*application-frame*)
		      (read-sample undo-name))
		*undo-level* level "wav"))
		


(define-wav-function redo (&optional (level nil))
  "Redo the effect of set-sample"
  (general-redo (setf (cl-wav-synth-clim::sample cl-wav-synth-clim::*application-frame*)
		      (read-sample redo-name))
		*undo-level* level "wav"))


(define-wav-function clear-undo (&optional (max-level 100))
  "Clear all undo/redo saved states"
  (generel-clear-undo *undo-level* "wav" max-level))


(define-wav-function list-undo (&optional (max-level 100))
  "List all undo/redo saved states"
  (general-list-undo *undo-level* "wav" max-level))







;;; Flags functions
(defun find-flag (number)
  (let ((map-sample (clim:find-pane-named cl-wav-synth-clim::*application-frame* 'cl-wav-synth-clim::map-sample)))
    (dolist (flag (cl-wav-synth-clim::flag-list map-sample))
      (when (= (cl-wav-synth-clim::flag-number flag) number)
	(return-from find-flag flag)))))


(define-wav-function fi (number)
  "Return the index of the sample flag 'number' (setfable)"
  (let ((flag (find-flag number)))
    (when flag
      (cl-wav-synth-clim::flag-index flag))))

(defun set-fi (number index)
  (let ((flag (find-flag number)))
    (when flag
      (setf (slot-value flag 'cl-wav-synth-clim::index) (truncate index)))))

(defsetf fi set-fi)



(define-wav-function ft (number)
  "Return the time of the sample flag 'number' (setfable)"
  (let ((flag (find-flag number)))
    (when flag
      (float (cl-wav-synth-clim::flag-time flag)))))

(defun set-ft (number time)
  (let ((flag (find-flag number)))
    (when flag
      (setf (slot-value flag 'cl-wav-synth-clim::index)
	    (truncate (cl-wav-synth-clim::app-time-to-sample time))))))

  
(defsetf ft set-ft)




(define-wav-function fa (number)
  "Return the amplitude of the sample flag 'number' (setfable)"
  (let ((flag (find-flag number)))
    (when flag
      (cl-wav-synth-clim::flag-ampl flag))))

(defun set-fa (number ampl)
  (let ((flag (find-flag number)))
    (when flag
      (setf (slot-value flag 'cl-wav-synth-clim::ampl) (truncate ampl)))))

  
(defsetf fa set-fa)





(define-wav-function flag-cut (sample flag-number1 flag-number2)
  "Return a new sample without the cutted part between
flag-number1  flag-number2"
  (cut-i sample (min (fi flag-number1) (fi flag-number2))
	 (max (fi flag-number1) (fi flag-number2))))

(define-wav-function flag-copy (sample flag-number1 flag-number2)
  "Return a new sample build from the original sample between
flag-number1 and flag-number2"
  (copy-i sample (min (fi flag-number1) (fi flag-number2))
	  (max (fi flag-number1) (fi flag-number2))))

(define-wav-function flag-insert (sample flag-number1 flag-number2 flag-number3)
  "Return a new sample with part between flag-number1 and flag-number2
inserted at flag-number3 time"
  (insert-sample sample (flag-copy sample flag-number1 flag-number2)
		 (ft flag-number3)))


