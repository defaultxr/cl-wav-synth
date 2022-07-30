;;; CL-Wav-Synth -  Manipulate WAV files
;;;
;;; Copyright (C) 2006 Philippe Brochard (hocwp@free.fr)
;;;
;;; #Date#: Mon May  1 22:19:44 2006
;;;
;;; **********************************************
;;; The authors grant you the rights to distribute
;;; and use this software as governed by the terms
;;; of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html),
;;; known as the LLGPL.
;;; **********************************************

(in-package :cl-wav-synth)

;;; This is in cl-wav-synth for compilation priority
;;;(defparameter *wav-test-hash* (make-hash-table))

(defmacro define-wav-test (name args &body body)
  `(progn
    (setf (gethash ',name *wav-test-hash*) ',body)
    (defun ,name ,args
      ,@body)))




;;; Tests functions
(define-wav-function list-all-test (&optional (stream *standard-output*))
  "List all available tests"
  (maphash #'(lambda (name v)
	       (declare (ignore v))
	       (when (fboundp name)
		 (format stream "~A: ~A~%" name (documentation name 'function))))
	   *wav-test-hash*)
  (format stream "..................................................
Please, choose a test with the command
> (testN)     where N is the test you want to start~%"))


(define-wav-function print-test (num &optional (stream *standard-output*))
  "View the code for test 'num'"
  (let ((name (intern (format nil "TEST~A" num))))
    (when (fboundp name)
      (pprint `(defun ,name ()
		,@(gethash name *wav-test-hash*))
	      stream))))
  




(defun set-fun (data new)
  (declare (ignore data))
  new)


(define-wav-test test1 ()
  "Test read-sample, print-header and write-sample on a test WAV file"
  (let ((sample (read-sample "test.wav")))
    (print-header sample "test.wav")
    (write-sample "toto.wav" sample)
    (aud "test.wav toto.wav")))


(define-wav-test test2 ()
  "Test read-sample, print-header and write-sample on another test WAV file"
  (let ((sample (read-sample "arthur.wav")))
    (print-header sample)
    (write-sample "toto.wav" sample)
    (aud "toto.wav")))



(define-wav-test test3 ()
  "Test sample-make-noise on a one channel sample"
  (let ((sample (make-instance 'sample :n-channels 1
			       :n-samples-per-sec 22050
			       :n-bits-per-sample 16
			       :time 10)))
    (with-slots (max-ampl) sample
      (sample-make-noise sample #'set-fun 0 0 10 440
			 (* max-ampl -0.8) (* max-ampl 0.8))
      (play sample))))

(define-wav-test test4 ()
  "Test sample-make-noise and sample-make-sin on a two channels sample"
  (let ((sample (make-instance 'sample :n-channels 2
			       :n-samples-per-sec 44100
			       :n-bits-per-sample 32
			       :time 10)))
    (with-slots (max-ampl) sample
      (time (progn
	      (print-header sample)
	      (sample-make-noise sample #'set-fun 0 0 10 50
				 (* max-ampl -0.4) (* max-ampl 0.4))
	      (sample-make-noise sample #'+ 0 0 10 1000
				 (* max-ampl -0.9) (* max-ampl 0.9))
	      (dotimes (i 40)
		(sample-make-sin sample #'set-fun 1 (* i 0.25) (+ (* i 0.25) 0.1) 440
				 (* max-ampl 0.8) #'sin)
		(sample-make-sin sample #'+ 1 (+ (* i 0.25) 0.15) (+ (* i 0.25) 0.2) 1500
				 (* max-ampl 0.6) #'sin))))
      (play sample))))


(define-wav-test test5 ()
  "Test sample-make-square and sample-make-sin"
  (let ((sample (make-instance 'sample :n-channels 1
			       :n-samples-per-sec 22050
			       :n-bits-per-sample 16
			       :time 10)))
    (with-slots (max-ampl) sample
      (sample-make-square sample #'set-fun 0 0 10 50
			  (* max-ampl -0.8) (* max-ampl 0.8))
      (sample-make-sin sample #'+ 0 2 8 1000 (* max-ampl 0.2) #'sin)
      (aud sample))))



(define-wav-test test6 ()
  "Test sample-make-line*"
  (let ((sample (make-instance 'sample :n-channels 1
			       :n-samples-per-sec 22050
			       :n-bits-per-sample 16
			       :time 10)))
    (with-slots (max-ampl) sample
      (sample-make-line* sample #'set-fun 0 0 10 100
			 (mapcar #'(lambda (x)
				     (list (first x)
					   (* max-ampl (second x))))
				 '((0 0.1) (0.2 1) (0.3 1) (0.4 0.8)
				   (0.5 -0.1) (0.6 -1) (0.7 -1) (1 -0.3))))
      (snd sample))))


(define-wav-test test7 ()
  "Test sample-make-line and sample-make-line*"
  (let ((sample (make-instance 'sample :n-channels 1
			       :n-samples-per-sec 22050
			       :n-bits-per-sample 16
			       :time 10)))
    (with-slots (max-ampl) sample
      (sample-make-line sample #'set-fun 0 0 10 440
			(* max-ampl -0.8) (* max-ampl 0.8))
      (sample-make-line* sample #'* 0 2 8 1 '((0 0.1) (0.2 0.9) (0.3 1) (0.4 0.8)
					      (0.5 -0.1) (0.6 -0.8) (0.7 -0.9) (1 -0.1)))
      (aud sample))))


(define-wav-test test8 ()
  "Test four channels sample"
  (let ((sample (make-instance 'sample :n-channels 4
			       :n-samples-per-sec 22050
			       :n-bits-per-sample 16
			       :time 10)))
    (with-slots (max-ampl) sample
      (sample-make-sin sample #'set-fun 0 0 10 440 max-ampl #'sin)
      (sample-make-sin sample #'set-fun 1 1 10 880 max-ampl #'sin)
      (sample-make-sin sample #'set-fun 2 2 10 770 max-ampl #'sin)
      (sample-make-sin sample #'set-fun 3 3 10 600 max-ampl #'sin)
      (aud sample))))

(define-wav-test test9 ()
  "Test copy-sample -> selection of a small piece of sample"
  (let* ((filename "arthur.wav")
	 (sample (read-sample filename))
	 (new-sample (copy-sample sample :start 10 :end 11)))
    (print-header sample)
    (print-header new-sample)
    (write-sample "toto.wav" new-sample)
    (play filename "toto.wav")))


(define-wav-test test1000 (n-chan &key (s-p-s 22050) (b-p-s 16) (time 3) name)
  "Helper function to create sample test"
  (let ((sample (make-instance 'sample :n-channels n-chan
			       :n-samples-per-sec s-p-s
			       :n-bits-per-sample b-p-s
			       :time time
			       :name name)))
    (print-header sample)
    (with-slots (max-ampl) sample
      (when (> n-chan 0)
	(sample-make-sin sample #'set-fun 0 1   2   1 max-ampl #'sin))
      (when (> n-chan 1)
	(sample-make-sin sample #'set-fun 1 0.5 1.5 2 (* max-ampl 0.5) #'cos))
      (when (> n-chan 2)
	(sample-make-sin sample #'set-fun 2 1.5 2.5 1 max-ampl #'sin))
      (when (> n-chan 3)
	(sample-make-sin sample #'set-fun 3 1.3 1.7 1 max-ampl #'cos)))
    sample))



(define-wav-test test1005 (n-chan &key (s-p-s 22050) (b-p-s 16) (time 3))
  "Helper function to test copy-sample"
  (let ((sample (test1000 n-chan :s-p-s s-p-s :b-p-s b-p-s :time time
			  :name "original.wav")))
    (play sample
	  (copy-sample sample :start 1 :end 2 :name "new1.wav")
	  (copy-sample sample :end 2 :name "new2.wav")
	  (copy-sample sample :start 1 :name "new3.wav"))))


    
(define-wav-test test10 ()
  "Test copy-sample on a one channel sample"
  (test1005 1))

(define-wav-test test11 ()
  "Test copy-sample on a two channels / 32 bits sample"
  (test1005 2 :s-p-s 44100 :b-p-s 32))

(define-wav-test test12 ()
  "Test copy-sample on a three channels sample"
  (test1005 3))

(define-wav-test test13 ()
  "Test copy-sample on a four channels sample"
  (test1005 4))

(define-wav-test test14 ()
  "Test copy-sample on a five channels sample"
  (test1005 5))


(define-wav-test test1010 (n-chan &key (s-p-s 22050) (b-p-s 16) (time 3))
  "Helper function to test read-sample"
  (let ((sample (test1000 n-chan :s-p-s s-p-s :b-p-s b-p-s :time time)))
    (write-sample "original.wav" sample)
    (play "original.wav"
	  (read-sample "original.wav" :start 1 :end 2 :name "new1.wav")
	  (read-sample "original.wav" :end 2 :name "new2.wav")
	  (read-sample "original.wav" :start 1 :name "new3.wav"))))

(define-wav-test test15 ()
  "Test read-sample with start / end time on a one channel sample"
  (test1010 1))

(define-wav-test test16 ()
  "Test read-sample with start / end time on a two channel sample"
  (test1010 2))

(define-wav-test test17 ()
  "Test read-sample with start / end time on a three channel sample"
  (test1010 3))

(define-wav-test test18 ()
  "Test read-sample with start / end time on a four channel sample"
  (test1010 4))

(define-wav-test test19 ()
  "Test read-sample with start / end time on a five channel sample"
  (test1010 5))


(define-wav-test test1015 (n-chan &key (s-p-s 22050) (b-p-s 16) (time 3))
  "Helper function to test write sample"
  (let ((sample (test1000 n-chan :s-p-s s-p-s :b-p-s b-p-s :time time))
	(new-sample (copy-sample (test1000 n-chan :s-p-s s-p-s :b-p-s b-p-s :time time)
				 :start 1 :end 2)))
    (when (probe-file "test.wav")
      (delete-file "test.wav"))
    (write-sample "old.wav" sample)
    (write-sample "new.wav" new-sample)
    (write-sample "test.wav" sample)
    (play "test.wav" "old.wav" "new.wav")
    (write-sample "test.wav" new-sample :start 0)
    (play "test.wav" "old.wav" "new.wav")
    (write-sample "test.wav" new-sample :start 0.5)
    (play "test.wav" "old.wav" "new.wav")
    (write-sample "test.wav" new-sample :start 2.5)
    (play "test.wav" "old.wav" "new.wav")
    (write-sample "test.wav" new-sample :start 4)
    (play "test.wav" "old.wav" "new.wav")
    (delete-file "old.wav")
    (delete-file "new.wav")
    (delete-file "test.wav")))

(define-wav-test test20 ()
  "Test write-sample with start / end time on a one channel sample"
  (test1015 1))

(define-wav-test test21 ()
  "Test write-sample with start / end time on a two channel sample"
  (test1015 2))

(define-wav-test test22 ()
  "Test write-sample with start / end time on a three channel sample"
  (test1015 3))

(define-wav-test test23 ()
  "Test write-sample with start / end time on a four channel sample"
  (test1015 4))

(define-wav-test test24 ()
  "Test write-sample with start / end time on a five channel sample"
  (test1015 5))

(define-wav-test test25 ()
  "Test direct access to data sample"
  (let ((sample (make-instance 'sample :n-channels 1
			       :n-samples-per-sec 22050
			       :n-bits-per-sample 16
			       :time 10)))
    (with-slots (data) sample
      (loop for i from 0 to 3000 do
	    (setf (aref data i) 20000))
      (loop for i from 10000 to 30000 do
	    (setf (aref data i) 10000)))
    (write-sample "toto.wav" sample)
    (write-sample "toto.wav" sample :start 3)
    (write-sample "toto.wav" sample :start 7)
    (aud "toto.wav")
    (delete-file "toto.wav")))

(define-wav-test test26 ()
  "Test add channel on a sample"
  (let ((bomb (read-sample "bomb.wav"))
	new)
    (play bomb)
    (setf new (add-channel bomb 1 bomb))
    (play new)
    (setf new (add-channel new 2 bomb :start 1))
    (play new)
    (setf new (add-channel new 0 bomb :start 2))
    (play new)
    (setf new (add-channel new 1 bomb :start 1.5))
    (play new)))

(define-wav-test test27 ()
  "Test extract-channel from a sample"
  (let ((arthur (read-sample "arthur.wav")))
    (play arthur (extract-channel arthur 0))))

(define-wav-test test28 ()
  "Test add-channel (with start time) and extract-channel"
  (let ((bomb (read-sample "bomb.wav")))
    (setf bomb (add-channel bomb 1 bomb :start 1))
    (play bomb
	  (extract-channel bomb 0)
	  (extract-channel bomb 1))))


(define-wav-test test29 ()
  "Test mix-sample"
  (let ((synth (read-sample "synth.wav"))
	(art (read-sample "arthur.wav")))
    (setf (sample-n-samples-per-sec synth) 32000)
    (setf synth (add-channel synth 1 synth :start 1))
    (play (mix-sample art synth #'(lambda (index s1 s2 m1 m2)
				    (declare (ignorable index))
				    (+ (* s1 m1) (* s2 m2)))
		      :args '(1 0.8) :start 5))))

(define-wav-test test30 ()
  "An other mix-sample test"
  (labels ((mix (ind s1 s2 m1 m2)
	     (declare (ignorable ind))
	     (+ (* s1 m1) (* s2 m2))))
    (let ((synth (read-sample "synth.wav"))
	  (art (read-sample "arthur.wav")))
      (setf (sample-n-samples-per-sec synth) 32000)
      (setf synth (add-channel synth 1 synth :start 1))
      (write-sample "test.wav" art)
      (mix-sample "test.wav" synth #'mix :args '(1 0.4) :start 2)
      (mix-sample "test.wav" synth #'mix :args '(0.5 0.8) :start 8)
      (loop for time from 15 by 6 to 30 do
	    (mix-sample "test.wav" synth #'mix :args '(1 0.5)
			:start (+ time (random 0.5))))
      (play "test.wav")
      (delete-file "test.wav"))))




(define-wav-test test31 ()
  "Test apply-on-sample"
  (let ((art (read-sample "arthur.wav")))
    (play art
	  (apply-on-sample art #'(lambda (index x)
				   (if (oddp index)
				       (* 5 x)
				       x))
			   :start 5 :end 20))))
				   


    
(define-wav-test test32 ()
  "An other apply-on-sample test -> distortion"
  (let* ((test (read-sample "arthur.wav"))
	 (max-ampl (/ (sample-max-ampl test) 2)))
    (play test
	  (apply-on-sample test #'(lambda (index x)
				    (declare (ignore index))
				    (setf x (* x 100))
				    (cond ((>= x max-ampl) max-ampl)
					  ((<= x (- max-ampl)) (- max-ampl))
					  (t x)))
			   :start 3 :end 23))))
				   

(define-wav-test test33 ()
  "Test pitch-up"
  (let* ((synth (read-sample "synth.wav")))
    (play synth
	  (pitch-up synth 2)
	  (pitch-up synth 3)
	  (pitch-up synth 4)
	  (pitch-up synth 10))))

(define-wav-test test34 ()
  "Test pitch-down"
  (let* ((synth (read-sample "synth.wav")))
    (play synth
	  (pitch-down synth 2)
	  (pitch-down synth 3)
	  (pitch-down synth 4)
	  (pitch-down synth 10))))

(define-wav-test test35 ()
  "Test pitch (a mix of pitch-down and pitch-up)"
  (let* ((synth (read-sample "synth.wav")))
    (play synth
	  (pitch synth 1.5)
	  (pitch synth 3.7)
	  (pitch synth 0.7)
	  (pitch synth 0.35))))



(define-wav-test test36 ()
  "Test time<->freq convert sample to spectre"
  (let ((test (make-instance 'sample :n-channels 1
			     :n-bits-per-sample 16
			     :n-samples-per-sec 22050
			     :time 1))
	spectre)
    (time
     (with-slots (max-ampl) test
       (sample-make-sin test #'set-fun 0 0 10 400 max-ampl #'sin)
       (sample-make-sin test #'+ 0 0 10 800 (/ max-ampl 2) #'sin)))
    (sample-view test :min 0.2 :max 0.21)
    (time
     (setf spectre (time<->freq test)))
    (sample-view spectre :min 100 :max 900)
    (sample-view spectre :min 100 :max 900 :fun #'phase)))


(define-wav-test test40 ()
  "Test build song"
  (build-song "test-song.wav"
	      (list (make-instance 'song-sample :time -1
				   :form '(progn
					   (defparameter synth (read-sample "synth.wav"))
					   (defparameter bomb (read-sample "bomb.wav"))))
		    (make-instance 'song-sample :time -0.9
				   :form '(setf (sample-n-samples-per-sec bomb) 44100))
		    (make-instance 'song-sample :time 1 :form 'synth)
		    (make-instance 'song-sample :time 20 :form 'synth)
		    (make-instance 'song-sample :time 10 :form "synth.wav")
		    (make-instance 'song-sample :time 3 :form '(pitch-down synth 3))
		    (make-instance 'song-sample :time 7 :form '(pitch-up synth 2))
		    (make-instance 'song-sample :time 15 :form 'bomb)
		    (make-instance 'song-sample :time 17 :form '(pitch-down bomb 2))
		    (make-instance 'song-sample :time 0.5 :form '(pitch-down bomb 25))))
  (aud "test-song.wav"))

(define-wav-test test41 ()
  "Test build song (2)"
  (let ((song (list (make-instance 'song-sample :time 1 :form 'synth)
		    (make-instance 'song-sample :time 10 :form "synth.wav")
		    (make-instance 'song-sample :time -1
				   :form '(progn
					   (defparameter synth (read-sample "synth.wav"))
					   (defparameter bomb (read-sample "bomb.wav"))))
		    (make-instance 'song-sample :time -0.9
				   :form '(setf (sample-n-samples-per-sec bomb) 44100)))))
    (loop for i from 1 to 10 do
	  (push (make-instance 'song-sample :time i :form `(pitch bomb (/ ,i 5))) song))
    (build-song "test-song.wav" song)
    (aud "test-song.wav")))


(define-wav-test test42 ()
  "Test build song (3)"
  (build-song "test-song.wav"
	      (list (make-instance 'song-sample :time -1
				   :form '(defvar synth (read-sample "synth.wav")))
		    (make-instance 'song-sample :time -0.9
				   :form '(defvar bomb (read-sample "bomb.wav")))
		    (make-instance 'song-sample :time -0.8
				   :form '(setf (sample-n-samples-per-sec bomb) 44100))
		    (make-instance 'song-sample :time 1 :form 'synth)
		    (make-instance 'song-sample :time 20 :form 'synth)
		    (make-instance 'song-sample :time 10 :form "synth.wav")
		    (make-instance 'song-sample :time 3 :form '(pitch-down synth 3))
		    (make-instance 'song-sample :time 7 :form '(pitch-up synth 2))
		    (make-instance 'song-sample :time 15 :form 'bomb)
		    (make-instance 'song-sample :time 17 :form '(pitch-down bomb 2))
		    (make-instance 'song-sample :time 0.5 :form '(pitch-down bomb 25))))
  (aud "test-song.wav"))



(define-wav-test test43 ()
  "Test build song form list"
  (build-song "test-song.wav"
	      (list-to-song '((-1 (defparameter synth (read-sample "synth.wav")))
			      (-0.9 (defparameter bomb (read-sample "bomb.wav")))
			      (-0.8 (setf (sample-n-samples-per-sec bomb) 44100) 1 (:plop :plip))
			      (1 synth)
			      (2 bomb))))
  (aud "test-song.wav"))

(define-wav-test test44 ()
  "Test with-build-song"
  (with-build-song ("test-song.wav")
    (-1   (defparameter synth (read-sample "synth.wav")))
    (-0.9 (defparameter bomb (read-sample "bomb.wav")))
    (-0.8 (setf (sample-n-samples-per-sec bomb) 44100) 1 (:plop :plip))
    (1    (pitch-up synth 2))
    (2    bomb)
    (3    "synth.wav"))
  (aud "test-song.wav"))

(define-wav-test test45 ()
  "Test with-song"
  (build-song "test-song.wav"
	      (with-list-song ()
		(-1   (defparameter synth (read-sample "synth.wav")) 0 :pouf 100)
		(-0.9 (defparameter bomb (read-sample "bomb.wav")))
		(-0.8 (setf (sample-n-samples-per-sec bomb) 44100) 1 (:plop :plip))
		(1    (pitch-up synth 2))
		(2    bomb)
		(3    "synth.wav")))
  (aud "test-song.wav"))

(define-wav-test test46 ()
  "Test write-song"
  (write-song "test-song.song"
	      (with-list-song ()
		(-1   (defparameter synth (read-sample "synth.wav")))
		(-0.9 (defparameter bomb (read-sample "bomb.wav")))
		(-0.8 (setf (sample-n-samples-per-sec bomb) 44100) 1 (:plop :plip))
		(1    (pitch-up synth 2))
		(2    bomb)
		(3    "synth.wav"))))

(define-wav-test test47 ()
  "Test read-song"
  (read-song "test-song.song")
  (build-song "test-song.wav" *current-song*)
  (aud "test-song.wav"))



