;;; CL-Wav-Synth -  Manipulate WAV files
;;;
;;; Copyright (C) 2006 Philippe Brochard (hocwp@free.fr)
;;;
;;; #Date#: Mon Jan 29 17:18:32 2007
;;;
;;; **********************************************
;;; The authors grant you the rights to distribute
;;; and use this software as governed by the terms
;;; of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html),
;;; known as the LLGPL.
;;; **********************************************

(defparameter *base-dir* (directory-namestring *load-truename*))

#+CMU
(setf ext:*gc-verbose* nil)


#+SBCL
(require :asdf)

#-ASDF
(load (make-pathname :host (pathname-host *base-dir*)
		     :device (pathname-device *base-dir*)
		     :directory (pathname-directory *base-dir*)
		     :name "asdf" :type "lisp"))

(push *base-dir* asdf:*central-registry*)


(asdf:oos 'asdf:load-op :cl-wav-synth)

#+CLIM
(asdf:oos 'asdf:load-op :cl-wav-synth-clim)


(in-package :cl-wav-synth)

#+CLIM
(cl-wav-synth-clim:run)

;;(wav::test100)
;;(wav::test105)
;;(wav::test45)
;;(wav::test46)
;;(wav::test47)
;;(test140)


(in-package :common-lisp-user)

;;#+CLIM
;;(quit)
