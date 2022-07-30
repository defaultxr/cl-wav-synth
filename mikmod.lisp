;;; CL-Wav-Synth -  Manipulate WAV files
;;;
;;; Copyright (C) 2006 Philippe Brochard (hocwp@free.fr)
;;;
;;; #Date#: Fri Feb  2 22:39:36 2007
;;;
;;; **********************************************
;;; The authors grant you the rights to distribute
;;; and use this software as governed by the terms
;;; of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html),
;;; known as the LLGPL.
;;; **********************************************


(in-package :cl-user)

(defpackage :mikmod
  (:nicknames :mm)
  (:use :common-lisp :cffi)
  (:export :mikmod-play-file
	   :*md-mode*
	   :*mikmod-control-function*
	   :mikmod-init
	   :mikmod-exit
	   :mikmod-enable-output
	   :mikmod-disable-output
	   :mikmod-set-num-voices
	   :mikmod-update
	   :mikmod-load
	   :mikmod-free
	   :mikmod-play
	   :mikmod-voice-stopped
	   :mikmod-voice-set-panning))
   
(in-package :mikmod)


(defparameter *mikmod-control-function* (lambda () nil)
  "A function returning T to stop the mikmod player")
   
(define-foreign-library libmikmod
  (:unix "libmikmod.so")
  (t (:default "mikmod")))

(use-foreign-library libmikmod)

(defcfun ("MikMod_RegisterAllDrivers" mikmod-register-all-drivers) :void)

(defcvar "md_mode" :unsigned-short)

(defcfun "MikMod_Init" :int (str :string))
(defcfun "MikMod_Exit" :void)


(defcfun ("MikMod_EnableOutput" mikmod-enable-output) :int)
(defcfun ("MikMod_DisableOutput" mikmod-disable-output) :void)

(defcfun ("MikMod_SetNumVoices" mikmod-set-num-voices) :int
  (musicvoices :int)
  (samplevoices :int))

(defcfun "MikMod_Update" :void)

(defcfun ("Sample_Load" mikmod-load) :pointer
  (filename :string))

(defcfun ("Sample_Free" mikmod-free) :void
  (sample :pointer))

(defcfun ("Sample_Play" mikmod-play) :char
  (sample :pointer)
  (start :unsigned-long)
  (flags :char))

(defcfun ("Voice_Stopped" mikmod-voice-stopped) :int
  (voice :int))


(defcfun ("Voice_SetPanning" mikmod-voice-set-panning) :void
  (voice :char)
  (panning :unsigned-int))




;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Beging of code
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Register all drivers once at load time
(mikmod-register-all-drivers)


(defun mikmod-play-file (filename)
  "Play a wav sample with mikmod (volume is in range 0 127)"
  (setf *md-mode* (logior *md-mode* #x0004))
  (unless (zerop (mikmod-init ""))
    (print "Could not initialize sound")
    (return-from mikmod-play-file nil))
  (unwind-protect
       (let ((sample (mikmod-load filename)))
	 (mikmod-set-num-voices -1 2)
	 (mikmod-enable-output)
	 (let ((voice (mikmod-play sample 0 0)))
	   (mikmod-voice-set-panning voice 127)
	   (mikmod-update)
	   (loop while (zerop (mikmod-voice-stopped voice)) do
		 (mikmod-update)
		 (when (funcall *mikmod-control-function*)
		   (return))
		 (sleep 0.1)))
	 (mikmod-free sample))
    (mikmod-disable-output)
    (mikmod-exit)))


(defun mikmod-test ()
  (setf *md-mode* (logior *md-mode* #x0004))
  (unless (zerop (mikmod-init ""))
    (print "Could not initialize sound")
    (return-from mikmod-test nil))
  (unwind-protect
       (let ((sample (mikmod-load "synth.wav"))
	     (sa2 (mikmod-load "synth.wav")))
	 (mikmod-set-num-voices -1 2)
	 (mikmod-enable-output)
	 (let ((voice (mikmod-play sample 0 0)))
	   (mikmod-voice-set-panning voice 127)
	   (mikmod-update)
	   (dotimes (i 40)
	     (mikmod-update)
	     (sleep 0.1)))
	 (let ((v2 (mikmod-play sa2 0 0)))
	   (mikmod-voice-set-panning v2 127)
	   (mikmod-update)
	   (dotimes (i 40)
	     (mikmod-update)
	     (sleep 0.1)))
	 (mikmod-free sample)
	 (mikmod-free sa2))
    (mikmod-disable-output)
    (mikmod-exit)))



(pushnew :mikmod *features*)
