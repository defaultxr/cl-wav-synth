;;; CL-Wav-Synth -  Manipulate WAV files
;;;
;;; Copyright (C) 2006 Philippe Brochard (hocwp@free.fr)
;;;
;;; #Date#: Fri Apr 28 16:38:35 2006
;;;
;;; **********************************************
;;; The authors grant you the rights to distribute
;;; and use this software as governed by the terms
;;; of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html),
;;; known as the LLGPL.
;;; **********************************************


(in-package :cl-wav-synth)


(define-wav-test test100 ()
  "Simple Clim test: display a sample"
  (let ((test (read-sample "bomb.wav")))
    (wav-clim:wav-clim test)))


(define-wav-test test101 ()
  "Simple Clim test: display a spectre"
  (let ((test (time<->freq (read-sample "synth.wav"))))
    (wav-clim:wav-clim test)))

(define-wav-test test102 ()
  "Simple Clim test: display a sample build from frequences"
  (wav-clim:wav-clim (build-from-freq '((440 0.5 0) (880 0.3 0))
				      :time (* 4 (/ 440)))))


(define-wav-test test103 ()
  "Simple Clim test: display a sample build from frequences (2)"
  (wav-clim:wav-clim (build-from-freq '((440 0.5 0) (880 0.3 0) (1320 0.2 0) (1760 0.34 0)))))

(define-wav-test test104 ()
  "Simple Clim test: display a sample build from frequences (3)"
  (wav-clim:wav-clim (build-from-freq
		      (loop for i from 1 to 10
			    collect (list (* 440 i)
					  (/ i)
					  0)))))

(define-wav-test test105 ()
  "Simple Clim test: display a sample build from frequences"
  (wav-clim:wav-clim '((1000 0.5 0.5) (1500 0.2 0.3) (3000 0.1 0) (3500 0.001 0))))

(define-wav-test test106 ()
  "Simple Clim test: display a sample build from frequences"
  (wav-clim:wav-clim (loop for i from 1 to 10
			   collect (list (* 440 i)
					 (/ i)
					 0))))


(define-wav-test test120 ()
  "Test write and read 16 bits"
  (let ((sample (make-instance 'sample :n-channels 1
			       :n-samples-per-sec 22050
			       :n-bits-per-sample 16
			       :time 2)))
    (with-slots (max-ampl) sample
      (sample-make-sin sample #'set-fun 1 0 1.99 440
		       (* max-ampl 0.8) #'sin)
      (write-sample "test-toto.wav" sample)
      (wav-clim:wav-clim (read-sample "test-toto.wav")))))

(define-wav-test test121 ()
  "Test write and read 8 bits"
  (let ((sample (make-instance 'sample :n-channels 1
			       :n-samples-per-sec 22050
			       :n-bits-per-sample 8
			       :time 2)))
    (with-slots (max-ampl) sample
      (sample-make-sin sample #'set-fun 1 0 1.99 440
		       (* max-ampl 0.8) #'sin)
      (write-sample "test-toto.wav" sample)
      (wav-clim:wav-clim (read-sample "test-toto.wav")))))

(define-wav-test test122 ()
  "Test write and read 32 bits"
  (let ((sample (make-instance 'sample :n-channels 1
			       :n-samples-per-sec 22050
			       :n-bits-per-sample 32
			       :time 2)))
    (with-slots (max-ampl) sample
      (sample-make-sin sample #'set-fun 1 0 1.99 440
		       (* max-ampl 0.8) #'sin)
      (write-sample "test-toto.wav" sample)
      (wav-clim:wav-clim (read-sample "test-toto.wav")))))


(define-wav-test test123 ()
  "Test write, read and append 16 bits"
  (let ((sample (make-instance 'sample :n-channels 1
			       :n-samples-per-sec 22050
			       :n-bits-per-sample 16
			       :time 0.5)))
    (with-slots (max-ampl) sample
      (sample-make-sin sample #'set-fun 1 0 0.49 440
		       (* max-ampl 0.8) #'sin)
      (write-sample "test-toto.wav" sample)
      (write-sample "test-toto.wav" sample :start 1)
      (wav-clim:wav-clim (read-sample "test-toto.wav")))))

(define-wav-test test124 ()
  "Test write, read and append 8 bits"
  (let ((sample (make-instance 'sample :n-channels 1
			       :n-samples-per-sec 22050
			       :n-bits-per-sample 8
			       :time 0.5)))
    (with-slots (max-ampl) sample
      (sample-make-sin sample #'set-fun 1 0 0.49 440
		       (* max-ampl 0.8) #'sin)
      (write-sample "test-toto.wav" sample)
      (write-sample "test-toto.wav" sample :start 1)
      (wav-clim:wav-clim (read-sample "test-toto.wav")))))

(define-wav-test test125 ()
  "Test write, read and append 32 bits"
  (let ((sample (make-instance 'sample :n-channels 1
			       :n-samples-per-sec 22050
			       :n-bits-per-sample 32
			       :time 0.5)))
    (with-slots (max-ampl) sample
      (sample-make-sin sample #'set-fun 1 0 0.49 440
		       (* max-ampl 0.8) #'sin)
      (write-sample "test-toto.wav" sample)
      (write-sample "test-toto.wav" sample :start 1)
      (wav-clim:wav-clim (read-sample "test-toto.wav")))))



(define-wav-test test140 ()
  "Test build song"
  (wav-clim:wav-clim-song 
   (with-list-song ()
     (-1   (defparameter synth (read-sample "synth.wav")))
     (-0.9 (defparameter bomb (read-sample "bomb.wav")))
     (-0.8 (setf (sample-n-samples-per-sec bomb) 44100) 1 (:plop :plip))
     (1    (pitch-up synth 2))
     (2    bomb)
     (3    "synth.wav"))))



