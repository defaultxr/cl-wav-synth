;;;; -*- Mode: Lisp -*-
;;;; Author: Philippe Brochard <hocwp@free.fr>
;;;; ASDF System Definition
;;;
;;; #date#: Fri Feb  2 23:40:50 2007

(in-package #:asdf)

(defsystem #:cl-wav-synth-clim
  :description "CL-WAV-Synth: manipulate wav files - CLIM sub-system"
  :version "0.5"
  :author "Philippe Brochard <hocwp@free.fr>"
  :maintainer "modula t. <defaultxr at gmail dot com>"
  :licence "Lisp Lesser GNU Public License (LLGPL)"
  :depends-on (#:cl-wav-synth
               #:clim
               #:clim-listener)
  :components ((:file "bezier")
	       (:file "cl-wav-synth-clim"
                :depends-on ("bezier"))
	       (:file "clim-sample-flags"
                :depends-on ("cl-wav-synth-clim"))
	       (:file "clim-sample"
                :depends-on ("cl-wav-synth-clim"
                             "clim-sample-flags"))
	       (:file "clim-song"
                :depends-on ("cl-wav-synth-clim"))
	       (:file "clim-facilities"
                :depends-on ("cl-wav-synth-clim"
                             "clim-sample-flags"
                             "clim-sample"
                             "clim-song"))
	       (:file "clim-sample-env"
                :depends-on ("cl-wav-synth-clim"
                             "clim-sample"
                             "clim-facilities"))
	       (:file "clim-song-selection"
                :depends-on ("cl-wav-synth-clim"
                             "clim-song"
                             "clim-facilities"))
	       (:file "clim-undo-redo"
                :depends-on ("cl-wav-synth-clim"
                             "clim-song"
                             "clim-sample"
                             "clim-facilities"))
	       (:file "test-clim"
                :depends-on ("cl-wav-synth-clim"
                             "clim-sample"
                             "clim-song"
                             "clim-facilities"))
	       (:file "clim-tutorial"
                :depends-on ("cl-wav-synth-clim"
                             "clim-sample"
                             "clim-song"
                             "clim-facilities"
                             "clim-undo-redo"
                             "test-clim"))
	       #+(and CFFI (not MIKMOD)) (:file "mikmod")
	       (:file "clim-song-recorder"
                :depends-on ("cl-wav-synth-clim"
                             "clim-sample"
                             "clim-song"
                             "clim-facilities"
                             #+(and CFFI (not MIKMOD)) "mikmod"))))
