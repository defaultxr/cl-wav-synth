;;;; -*- Mode: Lisp -*-
;;;; Author: Philippe Brochard <hocwp@free.fr>
;;;; ASDF System Definition
;;;
;;; #date#: Fri Feb  2 23:16:12 2007

(in-package #:asdf)

(defsystem #:cl-wav-synth
  :description "CL-WAV-Synth: manipulate wav files"
  :version "0.5"
  :author "Philippe Brochard <hocwp@free.fr>"
  :maintainer "modula t. <defaultxr at gmail dot com>"
  :licence "Lisp Lesser GNU Public License (LLGPL)"
  :components ((:file "uni-shell")
               (:file "bezier")
               #+CFFI (:file "mikmod")
               (:file "cl-wav-synth"
                :depends-on ("uni-shell"
                             "bezier"))
               (:file "cl-wav-synth-effect"
                :depends-on ("cl-wav-synth"
                             #+CFFI "mikmod"))
               (:file "test"
                :depends-on ("cl-wav-synth"
                             "cl-wav-synth-effect"))))
