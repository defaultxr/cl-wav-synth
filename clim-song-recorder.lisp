;;; CL-Wav-Synth -  Manipulate WAV files
;;;
;;; Copyright (C) 2006 Philippe Brochard (hocwp@free.fr)
;;;
;;; #Date#: Mon Feb  5 14:56:54 2007
;;;
;;; **********************************************
;;; The authors grant you the rights to distribute
;;; and use this software as governed by the terms
;;; of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html),
;;; known as the LLGPL.
;;; **********************************************


(in-package :cl-wav-synth)

(defclass song-key ()
  ((song-sample :initarg :song-sample :initform nil :accessor sk-song-sample)
   (mikmod-sample :initarg :mikmod-sample :initform nil :accessor sk-mm-sample)))


(defparameter *current-key-song* (make-hash-table))

(defun ask-song-sample (field message &optional default)
  (clim:with-application-frame (frame)
    (clim:accept field :stream (clim:frame-standard-input frame)
		 :prompt message :default default
		 :insert-default t)))


(defun convert-to-char (obj)
  "Convert obj to a character."
  (if obj
      (typecase obj
	(character obj)
	(string (aref obj 0))
	(symbol (convert-to-char (string-downcase (string obj)))))
      (progn
	(format t "Please, hit a key to define~%")
	(force-output)
	(clim:stream-read-char *standard-input*))))


(define-wav-function add-song-key (&key char form pos tags color)
  "Define a new key for the song recorder.
char is a character or it'll be asked from a keypress.
form, pos, tags and color are the attribute of a song-sample"
  (let ((n-char (convert-to-char char)))
    (format t "Defining: ~S~%" n-char)
    (force-output)
    (let ((song-sample (make-instance 'song-sample :time 0
				      :form (or form (ask-song-sample 'wav-clim::form "Form"))
				      :pos (or pos (ask-song-sample 'number "Position" 0))
				      :tags (or tags (ask-song-sample 'wav-clim::form "Tags"))
				      :color (or color (ask-song-sample 'number "Color" #x00FF00)))))
      (format t "Defining ~S:~%" n-char)
      (wav-clim::com-show-song-sample song-sample)
      (force-output)
      (setf (gethash n-char *current-key-song*)
	    (make-instance 'song-key :song-sample song-sample)))))


(define-wav-function add-song-key-tone (form &rest def)
  "Define some new keys for the song recorder.
Def is a list like this '(char1 keyword) '(char2 keyword2) ...
charN is a key to press in the recorder.
keywordN is a tone (:do :re ... or :c :d ...)"
  (let ((n-form (typecase form
		  (string `(read-sample ,form))
		  (t form))))
    (dolist (key def)
      (add-song-key :char (convert-to-char (first key))
		    :form `(tone ,n-form :do ,(second key))))))

(define-wav-function add-song-key-octave (form def)
  "Define some new keys for the song recorder.
Def is a list of characters to bind. The first char is :do (or :c), the
second :re (or :d) and so on..."
  (apply #'add-song-key-tone form  (loop for char in def
					 for tone in *main-tone-list*
					 collect (list char tone))))



(define-wav-function edit-song-key (&key char form pos tags color)
  "Edit a key for the song recorder.
char is a character or it'll be asked from a keypress.
form, pos, tags and color are the attribute of a song-sample"
  (let ((n-char (convert-to-char char)))
    (format t "Editing: ~S~%" n-char)
    (force-output)
    (let ((song-key (gethash n-char *current-key-song*)))
      (if song-key
	  (let ((song-sample (sk-song-sample song-key)))
	    (setf (s-form song-sample) (or form (ask-song-sample 'wav-clim::form
								 "Form" (s-form song-sample)))
		  (s-pos song-sample) (or pos (ask-song-sample 'number
							       "Position" (s-pos song-sample)))
		  (s-tags song-sample) (or tags (ask-song-sample 'wav-clim::form
								 "Tags" (s-tags song-sample)))
		  (s-color song-sample) (or color (ask-song-sample 'number
								   "Color" (s-color song-sample)))))
	  (add-song-key :char n-char :form form :pos pos :tags tags :color color)))))


(define-wav-function remove-song-key (&optional char)
  "Remove a key for the song recorder"
  (let ((n-char (convert-to-char char)))
    (format t "Removing: ~S~%" n-char)
    (force-output)
    (let ((song-sample (gethash n-char *current-key-song*)))
      (when song-sample
	(remhash n-char *current-key-song*)))))



(define-wav-function show-song-recorder-info ()
  "Show all keys defined for the song recorder"
  (format t "Recording with the table:~%")
  (maphash (lambda (key val)
	     (format t "  ~S: ~S~%" key (s-form (sk-song-sample val))))
	   *current-key-song*)
  (format t "~%-*- Press ~S to stop recording -*-~%" #\Newline))




(defun check-all-song-keys ()
  (let ((test-sample nil))
    (maphash (lambda (key val)
	       (format t "Checking ~S -> ~S...~%" key (s-form (sk-song-sample val)))
	       (force-output)
	       (let ((sample (eval-song-sample-form (sk-song-sample val))))
		 (if (sample-p sample)
		     (progn
		       (setf (sk-mm-sample val) sample)
		       (if test-sample
			   (progn
			     (handler-case
				 (mix test-sample sample)
			       (error (err )
				 (format t "Error: ~A~%" err)
				 (force-output)
				 (return-from check-all-song-keys nil))))
			   (setf test-sample sample)))
		     (setf (sk-mm-sample val) nil))))
	     *current-key-song*))
  t)


#+mikmod
(defun register-all-song-keys ()
  (unless (check-all-song-keys)
    (return-from register-all-song-keys nil))
  (setf mikmod:*md-mode* (logior mikmod:*md-mode* #x0004))
  (unless (zerop (mikmod:mikmod-init ""))
    (print "Register-all-song-keys: Could not initialize sound")
    (return-from register-all-song-keys nil))
  (maphash (lambda (key val)
	     (format t "Compiling ~S -> ~S...~%" key (s-form (sk-song-sample val)))
	     (force-output)
	     (if (sample-p (sk-mm-sample val))
		 (progn
		   (write-sample "tmp-record.wav" (sk-mm-sample val))
		   (setf (sk-mm-sample val) (mikmod:mikmod-load "tmp-record.wav"))
		   (delete-file "tmp-record.wav"))
		 (setf (sk-mm-sample val) nil)))
	   *current-key-song*)
  (mikmod:mikmod-set-num-voices -1 30)
  (mikmod:mikmod-enable-output)
  (format t "Hit a key to start recording...~%")
  (force-output)
  (clim:stream-read-char *standard-input*)
  t)



#-mikmod
(defun register-all-song-keys ()
  (check-all-song-keys))



#+mikmod
(defun play-song-key (song-key)
  (when (sk-mm-sample song-key)
    (let ((voice (mikmod:mikmod-play (sk-mm-sample song-key) 0 0)))
      (mikmod:mikmod-voice-set-panning voice 127)
      (mikmod:mikmod-update))))


#+mikmod
(defun close-all-song-keys ()
  (maphash (lambda (key val)
	     (declare (ignore key))
	     (when (sk-mm-sample val)
	       (mikmod:mikmod-free (sk-mm-sample val))))
	   *current-key-song*)
  (mikmod:mikmod-disable-output)
  (mikmod:mikmod-exit))





(define-wav-function record-song (&optional (delay 0.01))
  "Record a new song with a virtual keyboard. You can define new
keys with add-song-key. Delay is the key granularity."
  (let ((song nil))
    (unless (register-all-song-keys)
      (return-from record-song nil))
    (show-song-recorder-info)
    (force-output)
    (loop with end-loop = nil
	  until end-loop
	  for time from 0 do
	  (loop while (clim:stream-listen *standard-input*) do
		(let ((char (clim:stream-read-char-no-hang *standard-input*)))
		  (when char
		    (setf end-loop (equal char #\Newline))
		    (let ((song-key (gethash char *current-key-song*)))
		      (when song-key
			(let ((song-sample (sk-song-sample song-key)))
			  (format t "~A => ~S~%" (* time delay) (s-form song-sample))
			  (force-output)
			  #+mikmod (play-song-key song-key)
			  (push (copy-song-sample song-sample :time (* time delay)) song)))))))
	  #+mikmod (mikmod:mikmod-update)
	  (sleep delay))
    #+mikmod (close-all-song-keys)
    song))



(define-wav-function save-song-keys (filename)
  "Save the current keys for the song recorder"
  (with-open-file (stream filename :direction :output
			  :if-exists :supersede
			  :if-does-not-exist :create)
    (format stream ";;; Song keys for the recorder   -*- lisp -*-~2%")
    (format stream "~S~2%" '(in-package :wav))
    (format stream "~S~2%" '(defparameter *current-key-song* (make-hash-table)))
    (maphash (lambda (key val)
	       (let ((song-sample (sk-song-sample val)))
		 (format stream "~S~%"
			 `(add-song-key :char ,key
			   :form ',(s-form song-sample)
			   :pos ,(s-pos song-sample)
			   :tags ',(s-tags song-sample)
			   :color ,(s-color song-sample)))))
	     *current-key-song*)))


(define-wav-function load-song-keys (filename)
  "Load keys for the song recorder"
  (load filename))


;;; CLIM command table

(in-package :cl-wav-synth-clim)


(define-command-table song-recorder-command-table)

(add-menu-item-to-command-table 'song-recorder-command-table "Informations" :divider nil)

(define-command (com-show-song-key-info :name t :menu t
					:command-table song-recorder-command-table)
    ()
  (cl-wav-synth::show-song-recorder-info))

(add-menu-item-to-command-table 'song-recorder-command-table "Operations" :divider nil)

(define-command (com-add-song-key :name t :menu t
				  :command-table song-recorder-command-table)
    ()
  (cl-wav-synth::add-song-key))

(define-command (com-edit-song-key :name t :menu t
				   :command-table song-recorder-command-table)
    ()
  (cl-wav-synth::edit-song-key))

(define-command (com-remove-song-key :name t :menu t
				     :command-table song-recorder-command-table)
    ()
  (cl-wav-synth::remove-song-key))


(add-menu-item-to-command-table 'song-recorder-command-table "Files" :divider nil)

(define-command (com-save-song-keys :name t :menu t
				    :command-table song-recorder-command-table)
    ((path 'pathname))
  (cl-wav-synth::save-song-keys path))

(define-command (com-load-song-keys :name t :menu t
				    :command-table song-recorder-command-table)
    ((path 'pathname))
  (cl-wav-synth::load-song-keys path))



(add-menu-item-to-command-table 'song-recorder-command-table "Recording" :divider nil)

(define-command (com-record-song :name t :menu t
				 :command-table song-recorder-command-table)
    ()
  (cl-wav-synth::set-song (cl-wav-synth::record-song)))



(add-command-table-to-listener 'song-recorder-command-table)



;;; CLIM Listener hack - Adding some mime type
(in-package :clim-listener)

(define-mime-type (text song-keys)
    (:extensions "keys"))

(defmethod mime-type-to-command ((mime-type text/song-keys) pathname)
  (values `(wav-clim::com-load-song-keys ,pathname)
          (format nil "Load as keys ~A" (file-namestring pathname))
          (format nil "Load as keys ~A" pathname)))
