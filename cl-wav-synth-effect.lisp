*;;; CL-Wav-Synth -  Manipulate WAV files
;;;
;;; Copyright (C) 2006 Philippe Brochard (hocwp@free.fr)
;;;
;;; #Date#: Mon Jan 29 22:06:08 2007
;;;
;;; **********************************************
;;; The authors grant you the rights to distribute
;;; and use this software as governed by the terms
;;; of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html),
;;; known as the LLGPL.
;;; **********************************************


(in-package :cl-wav-synth)

(defparameter *wav-function-hash* (make-hash-table))


(defun find-near (exp &optional (max-num 30) (max-den 30))
  "Return the nearest rational to exp(ected) with num and den in max range"
  (let ((min exp) (win -1))
    (loop for num from 1 to max-num do
	  (loop for den from 1 to max-den do
		(let ((epsilon (abs (- exp (/ num den)))))
		  (when (< epsilon min)
		    (setf min epsilon
			  win (/ num den))))))
    win))



(defmacro define-wav-function (name args &body body)
  "Define a new function with help and export generated"
  `(progn
    (export ',name)
    (setf (gethash ',name *wav-function-hash*) ',args)
    (defun ,name ,args
      ,@body)))

(defmacro define-wav-macro (name args &body body)
  "Define a new macro with help and export generated"
  `(progn
    (export ',name)
    (setf (gethash ',name *wav-function-hash*) ',args)
    (defmacro ,name ,args
      ,@body)))


(defun internal-help (&optional name (stream *standard-output*))
  "Get the default help or a help on function
  Use (help) or (help 'a_function_name) for more details"
  (if name
      (progn
	(format stream "~&~A ~A" name (gethash name *wav-function-hash*))
	(format stream "~&  ~A" (documentation name 'function))
	(format stream "~&")
	(force-output stream))
      (progn
	(help 'help stream)
	(format stream "~&Wav defined functions are: ")
	(maphash #'(lambda (k v)
		     (declare (ignore v))
		     (format stream "~A " k))
		 *wav-function-hash*)
	(format stream "~&")
	(force-output stream))))


(define-wav-function help (&optional name (stream *standard-output*))
  "Get the default help or a help on function
  Use (help) or (help 'a_function_name) for more details"
  (internal-help name stream))


(define-wav-function long-help (&optional (stream *standard-output*))
  "Get the help for each wav defined functions"
  (maphash #'(lambda (k v)
	       (declare (ignore v))
	       (help k stream))
	   *wav-function-hash*))


(defun auto-doc-text (filename)
  "automatically create a documentation of cl-wav-synth in text form"
  (with-open-file (stream filename :direction :output
			  :if-exists :supersede
			  :if-does-not-exist :create)
    (format stream "CL-WAV-SYNTH: Express noises as you think~%~%~%")
    (format stream "This is the documentation for functions accessible from the listener~%~%~%")
    (maphash #'(lambda (k v)
		 (declare (ignore v))
		 (help k stream)
		 (format stream "~%"))
	     *wav-function-hash*)
    (format stream "~%~%Available tests are:~%~%")
    (maphash #'(lambda (name v)
		 (declare (ignore v))
		 (when (fboundp name)
		   (format stream "~A: ~A~%" name (documentation name 'function))))
	     *wav-test-hash*)
    (format stream "~%~%(This documentation has been automatically generated with the
function auto-doc)~%~%")))


(defun escape-name (name)
  "Replace * in name by . for compatibility with html"
  (substitute #\. #\* (format nil "~A" name)))


(defun subst-string (new old string)
  (let ((pos (search old string)))
    (if pos
	(concatenate 'string (subseq string 0 pos) new
		     (subst-string new old
				   (subseq string (+ pos (length old)))))
	string)))
    
(defun escape-arg (name)
  "Replace & with &amp;  < with &lt;  > with &gt;"
  (subst-string "&lt;" "<"
		(subst-string "&gt;" ">"
			      (subst-string "&amp;" "&"
					    (if (stringp name)
						name
						(format nil "~S" name))))))



(defun auto-doc-html (filename)
  "automatically create a documentation of cl-wav-synth in html form"
  (with-open-file (stream filename :direction :output
			  :if-exists :supersede
			  :if-does-not-exist :create)
    (format stream "<?xml version=\"1.0\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
    \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\">
 <head>
 <title>cl-wav-synth</title>
 <meta http-equiv=\"Content-Type\" content=\"text/html; charset=ISO-8859-1\"/>
</head>

<body>~%")
    (format stream "<h1><a id=\"top\" name=\"top\">
CL-WAV-SYNTH: Express noises as you think
</a></h1>~%")
    (format stream "<p><a href=\"#functions\">[Functions]</a>
<a href=\"#tests\">[Tests]</a>
</p>~%")
    (format stream "<h2><a id=\"functions\" name=\"functions\">This is the documentation for
functions accessible from the listener</a></h2>")
    (format stream "<ul>~%")
    (maphash #'(lambda (k v)
		 (declare (ignore v))
		 (format stream "<li><a href=\"#~A\">~A</a></li>~%"
			 (escape-name k) k))
	     *wav-function-hash*)
    (format stream "</ul>~%")
    (maphash #'(lambda (k v)
		 (format stream "<p>")
		 (format stream "<a href=\"#top\" id=\"~A\" name=\"~A\">~A ~A</a><br/>~%"
			 (escape-name k) (escape-name k) k
			 (escape-arg v))
		 (format stream "~A<br/><br/>" (escape-arg (documentation k 'function)))
		 (format stream "</p>~%~%"))
	     *wav-function-hash*)
    (format stream "<h2><a id=\"tests\" name=\"tests\">Available tests are:</a></h2>~%")
    (format stream "<ul>~%")
    (maphash #'(lambda (name v)
		 (declare (ignore v))
		 (when (fboundp name)
		   (format stream "<li>~A: ~A</li>~%" name
			   (escape-arg (documentation name 'function)))))
	     *wav-test-hash*)
    (format stream "</ul>~%")
    (format stream "<p><small>(This documentation has been automatically generated with the
function auto-doc)</small></p>~%~%")
    (multiple-value-bind (s min h d m y)
	(get-decoded-time)
      (declare (ignore s min h))
      (format stream "<div class=\"footer\">
     <a href=\"mailto:hocwp@free.fr\">Philippe Brochard</a>, ~A ~2,'0D ~2,'0D
   </div>

   <div class=\"check\">
     <a href=\"http://validator.w3.org/check/referer\">

        Valid XHTML 1.0 Strict</a>
  </div>
 </body>
</html>~%" y m d))))


(define-wav-function auto-doc (filename &optional (type :text))
  "Automatically generate the cl-wav-synth documentation from all exported functions and all tests.
Type can be :text or :html"
  (format t "~&Generating: ~A~%" filename)
  (ecase type
    (:text (auto-doc-text filename))
    (:html (auto-doc-html filename))))



;;; Simple mixing
(define-wav-function mix (sample1 sample2 &key (vol1 1) (vol2 1) (start 0))
  "Return a new sample with sample1 and sample2 mixed with volume vol1 and vol2,
  the second sample begin a time 'start' seconds"
  (mix-sample sample1 sample2 #'(lambda (index s1 s2 m1 m2)
				  (declare (ignorable index))
				  (+ (* s1 m1) (* s2 m2)))
	      :args (list vol1 vol2) :start start))




;;; Pitch functions
(define-wav-function pitch-up (sample mul)
  "Return a new sample pitched up"
  (with-slots (n-channels
	       n-bits-per-sample n-samples-per-sec data) sample
    (assert (and (integerp mul) (> mul 0)) (mul)
	    "error mul must be a positive integer and not ~A" mul)
    (assert (= n-channels 1) ()
	    "error sample must have exactly one channel and not ~A"
	    n-channels)
    (let* ((len (length data))
	   (new-len (truncate (/ len mul)))
	   (new-sample (make-instance 'sample :n-channels 1
				      :n-bits-per-sample n-bits-per-sample
				      :n-samples-per-sec n-samples-per-sec
				      :data (make-data new-len
						       n-bits-per-sample))))
      (set-sample-info new-sample)
      (set-total-byte-from-data new-sample)
      (set-last-sample new-sample)
      (with-slots ((new-data data)) new-sample
	(dotimes (i new-len)
	  (setf (aref new-data i)
		(truncate
		 (loop for j below mul
		       sum (aref data (+ (* i mul) j)) into sum
		       finally (return (/ sum mul)))))))
      new-sample)))


(define-wav-function pitch-down (sample mul)
  "Return a new sample pitched down"
  (with-slots (n-channels
	       n-bits-per-sample n-samples-per-sec data) sample
    (assert (and (integerp mul) (> mul 0)) (mul)
	    "error mul must be a positive integer and not ~A" mul)
    (assert (= n-channels 1) ()
	    "error sample must have exactly one channel and not ~A"
	    n-channels)
    (let* ((len (length data))
	   (new-len (* len mul))
	   (new-sample (make-instance 'sample :n-channels 1
				      :n-bits-per-sample n-bits-per-sample
				      :n-samples-per-sec n-samples-per-sec
				      :data (make-data new-len
						       n-bits-per-sample))))
      (set-sample-info new-sample)
      (set-total-byte-from-data new-sample)
      (set-last-sample new-sample)
      (with-slots ((new-data data)) new-sample
	(dotimes (i (1- len))
	  (loop for j below mul
		with dy = (/ (- (aref data (1+ i)) (aref data i)) mul) do
		(setf (aref new-data (+ (* i mul) j))
		      (truncate (+ (aref data i) (* j dy)))))))
      new-sample)))



(define-wav-function pitch (sample mul)
  "Return a new sample pitched with the mul value"
  (let* ((pitch (rationalize mul))
	 (num (numerator pitch))
	 (den (denominator pitch)))
    (pitch-up (pitch-down sample den) num)))



(define-wav-function local-pitch (sample pitch start end)
  "Return a new sample pitched by pitch between start and end seconds"
  (let ((new-sample (copy-sample sample :start 0 :end start)))
    (setf new-sample (mix new-sample
			  (pitch (copy-sample sample :start start :end end) pitch)
			  :start start))
    (mix new-sample (copy-sample sample :start end)
	 :start (sample-time new-sample))))



(defun smooth-sample (sample indexs)
  "Smooth sample in indexs list around each index"
  (with-slots (data max-index) sample
    (dolist (ind indexs)
      (when (< 1 ind (- max-index 3))
	(let* ((p1 (aref data (- ind 2)))
	       (p2 (aref data (+ ind 2)))
	       (dy (/ (- p2 p1) 4)))
	  (dotimes (i 3)
	    (setf (aref data (+ ind i -1)) (round (+ p1 (* dy (1+ i)))))))))))

(define-wav-function linear-pitch (sample start-pitch end-pitch start end
					  &key (interval 10) (max-num 30) (max-den 30))
  "Return a new sample with a linear pitch between start and end seconds
starting with start-pitch and ending with end-pitch."
  (let ((new-sample (copy-sample sample :start 0 :end start))
	(dtime (/ (- end start) interval))
	(dpitch (/ (- end-pitch start-pitch) interval))
	(indexs nil))
    (with-slots (n-samples-per-sec) new-sample
      (dotimes (i interval)
	(push (1- (time-to-sample n-samples-per-sec (sample-time new-sample))) indexs)
	(setf new-sample (mix new-sample
			      (pitch (copy-sample sample
						  :start (+ start (* i dtime))
						  :end (+ start (* (1+ i) dtime)))
				     (find-near (+ (* i dpitch) start-pitch) max-num max-den))
			      :start (max (- (sample-time new-sample) (sample-to-time (sample-n-samples-per-sec new-sample) 1)) 0))))
      (push (1- (time-to-sample n-samples-per-sec (sample-time new-sample))) indexs)
      (mix new-sample (copy-sample sample :start end)
	   :start (sample-time new-sample))
      (smooth-sample new-sample indexs)
      new-sample)))



(define-wav-function bezier-pitch (sample t1 p1 t2 p2 t3 p3 t4 p4
					  &key (interval 10) (max-num 30) (max-den 30))
  "Return a new sample pitched from t1 p1 to t4 p4 with two control points
t2 p2 and t3 p3"
  (let ((new-sample (copy-sample sample :start 0 :end t1))
	(indexs nil))
    (with-slots (n-samples-per-sec) new-sample
      (gen-bezier t1 p1 t2 p2 t3 p3 t4 p4)
      (with-bezier (start start-pitch end end-pitch)
	(let ((dtime (/ (- end start) interval))
	      (dpitch (/ (- end-pitch start-pitch) interval)))
	  (dotimes (i interval)
	    (push (1- (time-to-sample n-samples-per-sec (sample-time new-sample))) indexs)
	    (setf new-sample (mix new-sample
				  (pitch (copy-sample sample
						      :start (+ start (* i dtime))
						      :end (+ start (* (1+ i) dtime)))
					 (find-near (+ (* i dpitch) start-pitch) max-num max-den))
				  :start (max (- (sample-time new-sample) (sample-to-time (sample-n-samples-per-sec new-sample) 1)) 0))))))
      (push (1- (time-to-sample n-samples-per-sec (sample-time new-sample))) indexs)
      (mix new-sample (copy-sample sample :start t4)
	   :start (sample-time new-sample))
      (smooth-sample new-sample indexs)
      new-sample)))
    
  


;;; Stereo functions
(define-wav-function stereo (sample1 &optional sample2)
  "Return a new stereo sample with sample1 on channel 1 and
  sample2 on channel 2"
  (let* ((new-sample (copy-sample sample1))
	 (new-sample2 (or sample2 new-sample)))
    (add-channel new-sample 1 new-sample2)))

(define-wav-function stereo-pan (sample &optional (pan 0.5))
  "Return a new stereo sample with sample on channel 1 and 2 with
pan=0.5 -> center, pan<0.5 -> left, pan>0.5 -> right"
  (let* ((new-sample (volume sample (- 1 pan)))
	 (new-sample2 (volume sample pan)))
    (add-channel new-sample 1 new-sample2)))

(define-wav-function quadri (sample1 sample2 sample3 sample4)
  "Return a new quadri sample with sample1 on channel 1,
  sample2 on channel 2 and so on for channels 3 and 4"
  (let ((new-sample (copy-sample sample1)))
    (setf new-sample (add-channel new-sample 1 sample2))
    (setf new-sample (add-channel new-sample 2 sample3))
    (setf new-sample (add-channel new-sample 3 sample4))
    new-sample))



;;; Volume function
(defun borned-value (val max-ampl)
  (cond ((>= val max-ampl) max-ampl)
	((<= val (- max-ampl)) (- max-ampl))
	(t val)))

(define-wav-function volume (sample new-vol)
  "Return a new sample with volume multiplicated by new-vol"
  (with-slots (n-channels
	       n-bits-per-sample n-samples-per-sec data) sample
    (assert (and (numberp new-vol) (>= new-vol 0)) (new-vol)
	    "error new volume must be a positive number and not ~A" new-vol)
    (let* ((len (length data))
	   (new-sample (make-instance 'sample :n-channels 1
				      :n-bits-per-sample n-bits-per-sample
				      :n-samples-per-sec n-samples-per-sec
				      :data (make-data len n-bits-per-sample))))
      (set-sample-info new-sample)
      (set-total-byte-from-data new-sample)
      (set-last-sample new-sample)
      (with-slots ((new-data data) max-ampl) new-sample
	(dotimes (i len)
	  (setf (aref new-data i)
		(borned-value (truncate (* (aref data i) new-vol)) max-ampl))))
      new-sample)))



;;; Delay function
(define-wav-function delay (sample delay)
  "Return a new sample with a delay of 'delay' seconds"
  (assert (>= delay 0) ()
	  "error delay must be a null or positive time in seconds")
  (with-slots (n-channels
	       n-bits-per-sample
	       n-samples-per-sec
	       data) sample
    (let* ((s-delay (* (time-to-sample n-samples-per-sec delay)
		       n-channels))
	   (new-len (+ (length data) s-delay))
	   (new-sample (make-instance 'sample :n-channels n-channels
				      :n-bits-per-sample n-bits-per-sample
				      :n-samples-per-sec n-samples-per-sec
				      :data (make-data new-len
						       n-bits-per-sample))))
      (set-sample-info new-sample)
      (set-total-byte-from-data new-sample)
      (set-last-sample new-sample)
      (with-slots ((new-data data)) new-sample
	(dotimes (i (length data))
	  (setf (aref new-data (+ i s-delay))
		(aref data i))))
      new-sample)))


;;; Echo function
(define-wav-function echo (sample delay volumes)
  "Return a new sample repeatated each 'delay' seconds with volumes
  on the list volumes"
  (let ((new-sample (copy-sample sample))
	(start delay))
    (dolist (vol volumes)
      (setf new-sample (mix-sample new-sample sample
				   #'(lambda (index s1 s2 m1 m2)
				       (declare (ignorable index))
				       (+ (* s1 m1) (* s2 m2)))
				   :args (list 1 vol)
				   :start start))
      (incf start delay))
    new-sample))



;;; Cut/Insert/Copy functions
(define-wav-function cut (sample &optional start end)
  "Return a new sample without the cutted part from
start to end in seconds"
  (cut-sample sample start end))

(define-wav-function insert (sample sample2 start)
  "Return a new sample with sample2 inserted at start seconds"
  (insert-sample sample sample2 start))

(define-wav-function copy (sample &optional start end)
  "Return a new sample build from the original sample between
start and end seconds"
  (copy-sample sample :start start :end end))



(define-wav-function cut-i (sample &optional start-i end-i)
  "Return a new sample without the cutted part from
start to end in index numbers"
  (let ((start (s->t sample start-i))
	(end (s->t sample end-i)))
    (cut-sample sample start end)))

(define-wav-function insert-i (sample sample2 start-i)
  "Return a new sample with sample2 inserted at start in
index number"
  (let ((start (s->t sample start-i)))
    (insert-sample sample sample2 start)))


(define-wav-function copy-i (sample &optional start-i end-i)
  "Return a new sample build from the original sample between
start and end in index numbers"
  (let ((start (s->t sample start-i))
	(end (s->t sample end-i)))
    (copy-sample sample :start start :end end)))



;;; Envelope function
(defun sample-draw-line* (sample fun chan points)
  "Set sample envelope with a line. Points is a list like
   (list (index1 volume1) (index2 volume2) ...)"
  (labels ((line (mins maxs mina maxa)
	     (unless (= mins maxs)
	       (when (< maxs mins)
		 (rotatef mins maxs)
		 (rotatef mina maxa))
	       (let ((dy (/ (- maxa mina) (- maxs mins))))
		 (loop for i from mins below maxs do
		       (set-ampl sample fun chan i
				 (+ mina (* (- i mins) dy))))))))
    (loop for last = (first points) then p
	  for p in (cdr points) do
	  (line (t->s sample (first last))
		(t->s sample (first p))
		(second last) (second p)))))


(define-wav-function env-fun (sample points)
  "Set sample envelope. Points is a list like
   (list (time1 volume1) (time2 volume2) ...)"
  (let ((new-sample (copy-sample sample)))
    (sample-draw-line* new-sample #'* 0 points)
    new-sample))


(define-wav-macro env (sample &rest points)
  "Set sample envelope. Points are (time1 volume1) (time2 volume2)..."
  `(env-fun ,sample ',points))


(define-wav-function env-sinus (sample &key (start 0) end (period 1) (min -1) (max 1))
  "Set sample envelope with a sinus function. Start and end are in seconds,
  period is the number of periods between start and end and min/max are
  volume multiplicators"
  (let* ((new-sample (copy-sample sample))
	 (end (or end (sample-time sample)))
	 (period (/ (- end start) period))
	 (freq (/ period)))
    (labels ((sin-fun (x)
	       (+ (* (/ (- max min) 2) (sin x)) (/ (+ max min) 2))))
      (sample-make-sin new-sample #'* 0 start end freq 1 #'sin-fun)
      new-sample)))


(define-wav-function env-bezier (sample t1 v1 t2 v2 t3 v3 t4 v4)
  "Set sample envelope with a bezier curve with t1 v1 and t4 v4
as endpoints and t2 v2 and t3 v3 as control points"
  (let* ((new-sample (copy-sample sample)))
    (gen-bezier t1 v1 t2 v2 t3 v3 t4 v4)
    (with-bezier (x1 y1 x2 y2)
      (sample-draw-line* new-sample #'* 0 (list (list x1 y1) (list x2 y2))))
    new-sample))



;;; Generator functions
(define-wav-function null-sample (time &key (sps 22050) (bps 16))
  "Create a muted sample.
   sps = sample per seconds
   bps = bits per sample"
  (make-instance 'sample :n-channels 1
		 :n-samples-per-sec sps
		 :n-bits-per-sample bps
		 :time time))


(define-wav-function sinus-sample (time freq &key (sps 22050) (bps 16)
					(ampl 32767) (phase 0))
  "Create a sinus sample.
   sps = sample per seconds
   bps = bits per sample"
  (let ((new-sample (null-sample time :sps sps :bps bps)))
    (sample-make-sin new-sample #'set-fun 0 0 time freq ampl #'sin phase)
    new-sample))


(define-wav-function square-sample (time freq &key (sps 22050) (bps 16)
					 (min -32767) (max 32767))
  "Create a square sample.
   sps = sample per seconds
   bps = bits per sample
   min and max = amplitudes"
  (let ((new-sample (null-sample time :sps sps :bps bps)))
    (sample-make-square new-sample #'set-fun 0 0 time freq min max)
    new-sample))


(define-wav-function line-sample (time freq &key (sps 22050) (bps 16)
				       (min -32767) (max 32767))
  "Create a line sample.
   sps = sample per seconds
   bps = bits per sample
   min and max = amplitudes"
  (let ((new-sample (null-sample time :sps sps :bps bps)))
    (sample-make-line new-sample #'set-fun 0 0 time freq min max)
    new-sample))


(define-wav-function line-sample* (time freq &key (sps 22050) (bps 16)
					(ampls '((0 -32767) (1 32767))))
  "Create a line sample based on control points.
   sps = sample per seconds
   bps = bits per sample
   ampls = List of amplitudes like '((val1 ampl1) (val2 ampl2)...)
           where 0 (sample begining) <= val <= 1 (sample end)"
  (let ((new-sample (null-sample time :sps sps :bps bps)))
    (sample-make-line* new-sample #'set-fun 0 0 time freq ampls)
    new-sample))


(define-wav-function noise-sample (time freq &key (sps 22050) (bps 16)
					(min -32767) (max 32767))
  "Create a noise sample.
   sps = sample per seconds
   bps = bits per sample
   min and max = amplitudes"
  (let ((new-sample (null-sample time :sps sps :bps bps)))
    (sample-make-noise new-sample #'set-fun 0 0 time freq min max)
    new-sample))



;;; Modifier functions
(define-wav-function draw-env-fun (sample points)
  "Draw sample envelope. Points is a list like
   (list (time1 volume1) (time2 volume2) ...)"
  (let ((new-sample (copy-sample sample)))
    (sample-draw-line* new-sample #'(lambda (data new)
				      (declare (ignore data))
				      new)
		       0 points)
    new-sample))


(define-wav-macro draw-env (sample &rest points)
  "Draw sample envelope. Points are (time1 volume1) (time2 volume2)..."
  `(draw-env-fun ,sample ',points))


  
(define-wav-function draw-env-bezier (sample t1 a1 t2 a2 t3 a3 t4 a4)
  "Draw sample envelope with a bezier curve with t1 a1 and t4 a4
as endpoints and t2 a2 and t3 a3 as control points"
  (let* ((new-sample (copy-sample sample)))
    (gen-bezier t1 a1 t2 a2 t3 a3 t4 a4)
    (with-bezier (x1 y1 x2 y2)
      (sample-draw-line* new-sample #'(lambda (data new)
					(declare (ignore data))
					new)
			 0 (list (list x1 y1) (list x2 y2))))
    new-sample))




;;; Players
(define-wav-function player-info ()
  "Display informations about the current player"
  (format t "The current player is ~S~%" *default-player*)
  (help *default-player*)
  (force-output))


(defun cmd-play (cmd &rest args)
  (let* ((to-delete nil)
	 (newcmd (with-output-to-string (str)
		   (format str "~A " cmd)
		   (dolist (x args)
		     (format str "~A "
			     (if (eql (class-of x) (find-class 'sample))
				 (let ((name (format nil "tmp-~A"
						     (sample-name x))))
				   (push name to-delete)
				   (write-sample name x)
				   name)
				 x))))))
    (ush newcmd)
    (mapc #'delete-file to-delete))
  t)


#+MIKMOD
(define-wav-function mikmod-play (&rest args)
  "Play each samples with the mikmod library"
  (let* ((to-delete nil)
	 (newsamples (loop for x in args
			   collect (if (eql (class-of x) (find-class 'sample))
				       (let ((name (format nil "tmp-~A"
							   (sample-name x))))
					 (push name to-delete)
					 (write-sample name x)
					 name)
				       x))))
    (unwind-protect
	 (progn
	   (format t "Playing ~{~A~^, ~} with mikmod~%" newsamples)
	   (force-output)
	   (dolist (x newsamples)
	     (mikmod:mikmod-play-file x)))
      (mapc #'delete-file (remove-duplicates to-delete :test #'equal))))
  t)
  

(define-wav-function sox-play (&rest samples)
  "Call the sox play command on all samples"
  (apply #'cmd-play "play" samples))



(define-wav-function dplay (&rest samples)
  "Call the default player command on all samples with the mikmod library
  (if compiled in) or from the sox play command"
  #+MIKMOD (apply #'mikmod-play samples)
  #-MIKMOD (apply #'sox-play samples))


		
(define-wav-function xmms (&rest samples)
  "Call xmms on all samples (a sample class or a filename)"
  (apply #'cmd-play "xmms" samples))

(define-wav-function snd (&rest samples)
  "Call snd on all samples (a sample class or a filename)"
  (apply #'cmd-play "snd" samples))

(define-wav-function audacity (&rest samples)
  "Call audacity on all samples (a sample class or a filename)"
  (apply #'cmd-play "audacity" samples))

(define-wav-function aud (&rest samples)
  "Call audacity on all samples (a sample class or a filename)"
  (apply #'cmd-play "audacity" samples))

(define-wav-function cool-player (&rest samples)
  "Call cool-player on all samples (a sample class or a filename)"
  (apply #'cmd-play "coolplayer" samples))

(define-wav-function totem (&rest samples)
  "Call totem on all samples (a sample class or a filename)"
  (apply #'cmd-play "totem" samples))

(define-wav-function bmp (&rest samples)
  "Call beep-media-player on all samples (a sample class or a filename)"
  (apply #'cmd-play "beep-media-player" samples))

(define-wav-function macplay (&rest samples)
  "Call the Mac player on all samples (a sample class or a filename)"
  (apply #'cmd-play "/Applications/QuickTime\\ Player.app/Contents/MacOS/QuickTime\\ Player" samples))



(define-wav-function play (&rest samples)
  "Default player, call play or coolplayer on Windows on all samples
 (a sample class or a filename)"
  (handler-case 
      (if (stringp *default-player*)
	  (apply #'cmd-play *default-player* samples)
	  (apply *default-player* samples))
    (error (err) (format t "Error: can't play with ~A : ~A" *default-player* err))))




(define-wav-function set-player (&optional player)
  "Set the default player called with the 'play' function"
  (if player
      (setf *default-player* player)
      (progn
	(format t "The default player can be a symbol in:")
	(format t "  dplay, xmms, snd, audacity, cool-player, totem, bmp, macplay")
	(format t "  or a command line string"))))


;;; Reverse function
(define-wav-function reverse-sample (sample &key (start 0) end)
  "Return a reversed sample from time start to end in seconds"
  (with-slots (data max-index) sample
    (let ((new-sample (copy-sample sample)))
      (multiple-value-bind (i1 i2)
	  (find-smin-smax sample start end)
	(decf i2)
	(with-slots ((new-data data)) new-sample
	  (dotimes (i (- i2 i1))
	    (setf (aref new-data (+ i1 i))
		  (aref data (- i2 i))))))
      new-sample)))



;;; Filter functions
;; *-filter-ng functions are non generales functions. I let theme here
;; only for efficiency usage _if nedded_.

(defun trunc-bound-val (val max-ampl)
  (cond ((>= val max-ampl) max-ampl)
	((<= val (- max-ampl)) (- max-ampl))
	(t (truncate val))))

(define-wav-function filter (sample &optional (an '(1)) (bn '(0)))
  "Apply a general filter on sample.
Filter is: s(n) = a0 e(n) + a1 e(n-1) + a2 e(n-2) + ...
                + b0 s(n-1) + b1 s(n-2) + b3 s(n-3) + ...
an and bn are list of coefficients.
Example: (filter sample '(0.3 0.2 0.1) '(0.1 0.2 0.3))"
  (with-slots (n-samples-per-sec
	       max-index max-ampl
	       data) sample
    (let* ((new-sample (copy-sample sample))
	   (l-an (length an))
	   (l-bn (length bn))
	   (an (coerce (or an '(1)) 'vector))
	   (bn (coerce (or bn '(0)) 'vector))
	   (en-i (make-array (if (<= (1- l-an) 0) 1 (1- l-an)) :initial-element 0))
	   (sn-i (make-array (if (<= l-bn 0) 1 l-bn) :initial-element 0)))
      (format t "~&Aplying: s(n) = ")
      (dotimes (i l-an)
	(format t "~,3F e(n-~A)" (svref an i) i)
	(when (or (< i (1- l-an)) (plusp l-bn))
	  (format t " + ")))
      (dotimes (i l-bn)
	(format t "~,3F s(n-~A)" (svref bn i) (1+ i))
	(when (< i (1- l-bn))
	  (format t " + ")))
      (with-slots ((new-data data)) new-sample
	(dotimes (ind max-index)
	  (let ((val (* (aref data ind) (svref an 0))))
	    (dotimes (i (1- l-an))
	      (setf val (+ val (* (svref an (1+ i))
				  (svref en-i i)))))
	    (dotimes (i l-bn)
	      (setf val (+ val (* (svref bn i)
				  (svref sn-i i)))))
	    (setf (aref new-data ind)
		  (trunc-bound-val val max-ampl))
	    (dotimes (i (- l-an 2))
	      (setf (svref en-i (1+ i))
	    	    (svref en-i i)))
	    (setf (svref en-i 0) (aref data ind))
	    (dotimes (i (1- l-bn))
	      (setf (svref sn-i (1+ i))
		    (svref sn-i i)))
	    (setf (svref sn-i 0) val))))
      new-sample)))


(defun low-filter-ng (sample f0)
  "Apply a low filter to sample where f0 is the cut frequency
H(f) = 1/(1 + j f/f0) -> s(n) = a e(n) + (1-a) s(n-1)
a=Te/(tau+Te) with tau=1/(2 pi f0)"
  (with-slots (n-samples-per-sec
	       max-index max-ampl
	       data) sample
    (let* ((new-sample (copy-sample sample))
	   (tau (/ (* 2 pi f0)))
	   (Te (/ n-samples-per-sec))
	   (a (/ Te (+ tau Te)))
	   (sn-1 0))
      (with-slots ((new-data data)) new-sample
	(dotimes (i max-index)
	  (let ((val (+ (* a (aref data i))
			(* (- 1 a) sn-1))))
	    (setf (aref new-data i)
		  (trunc-bound-val val max-ampl))
	    (setf sn-1 val))))
      new-sample)))

(define-wav-function low-filter (sample f0)
  "Apply a low filter to sample where f0 is the cut frequency
H(f) = 1/(1 + j f/f0) -> s(n) = a e(n) + (1-a) s(n-1)
a=Te/(tau+Te) with tau=1/(2 pi f0)"
  (with-slots (n-samples-per-sec
	       max-index max-ampl
	       data) sample
    (let* ((tau (/ (* 2 pi f0)))
	   (Te (/ n-samples-per-sec))
	   (a (/ Te (+ tau Te))))
      (filter sample (list a) (list (- 1 a))))))



(defun high-filter-ng (sample f0)
  "Apply a high filter to sample where f0 is the cut frequency
H(f) = 1/(1 + j f0/f) -> s(n) = a e(n) - a e(n-1) + a s(n-1)
a=tau/(tau+Te) with tau=1/(2 pi f0)"
  (with-slots (n-samples-per-sec
	       max-index max-ampl
	       data) sample
    (let* ((new-sample (copy-sample sample))
	   (tau (/ (* 2 pi f0)))
	   (Te (/ n-samples-per-sec))
	   (a (/ tau (+ tau Te)))
	   (en-1 0)
	   (sn-1 0))
      (with-slots ((new-data data)) new-sample
	(dotimes (i max-index)
	  (let ((val (* a (+ (aref data i)
			     (- en-1)
			     sn-1))))
	    (setf (aref new-data i)
		  (trunc-bound-val val max-ampl))
	    (setf en-1 (aref data i))
	    (setf sn-1 val))))
      new-sample)))


(define-wav-function high-filter (sample f0)
  "Apply a high filter to sample where f0 is the cut frequency
H(f) = 1/(1 + j f0/f) -> s(n) = a e(n) - a e(n-1) + a s(n-1)
a=tau/(tau+Te) with tau=1/(2 pi f0)"
  (with-slots (n-samples-per-sec
	       max-index max-ampl
	       data) sample
    (let* ((tau (/ (* 2 pi f0)))
	   (Te (/ n-samples-per-sec))
	   (a (/ tau (+ tau Te))))
      (filter sample (list a (- a)) (list a)))))




(defun band-filter-ng (sample f0 &optional (Q 1))
  "Apply a band filter to sample where f0 is the cut frequency
and Q is the quality coefficient.
H(f) = 1/(1 + j Q (f/f0 - f0/f))
-> d s(n) = a e(n) - a e(n-1) + b s(n-1) + c s(n-2)
with a=tau/Te  b=a+2 Q^2 a^2  c=-Q a^2  d=a+Q+Q a^2"
  (with-slots (n-samples-per-sec
	       max-index max-ampl
	       data) sample
    (let* ((new-sample (copy-sample sample))
	   (tau (/ (* 2 pi f0)))
	   (Te (/ n-samples-per-sec))
	   (a (/ tau Te))
	   (b (+ a (* 2 Q Q a a)))
	   (c (- (* Q a a)))
	   (d (+ a Q (* Q a a)))
	   (en-1 0)
	   (sn-1 0)
	   (sn-2 0))
      (with-slots ((new-data data)) new-sample
	(dotimes (i max-index)
	  (let ((val (/ (+ (* (aref data i) a)
			   (* en-1 (- a))
			   (* sn-1 b)
			   (* sn-2 c))
			d)))
	    (setf (aref new-data i)
		  (trunc-bound-val val max-ampl))
	    (setf en-1 (aref data i))
	    (setf sn-2 sn-1)
	    (setf sn-1 val))))
      new-sample)))


(define-wav-function band-filter (sample f0 &optional (Q 1))
  "Apply a band filter to sample where f0 is the cut frequency
and Q is the quality coefficient.
H(f) = 1/(1 + j Q (f/f0 - f0/f))
-> d s(n) = a e(n) - a e(n-1) + b s(n-1) + c s(n-2)
with a=tau/Te  b=a+2 Q^2 a^2  c=-Q a^2  d=a+Q+Q a^2"
  (with-slots (n-samples-per-sec
	       max-index max-ampl
	       data) sample
    (let* ((tau (/ (* 2 pi f0)))
	   (Te (/ n-samples-per-sec))
	   (a (/ tau Te))
	   (b (+ a (* 2 Q Q a a)))
	   (c (- (* Q a a)))
	   (d (+ a Q (* Q a a))))
      (filter sample (list (/ a d) (- (/ a d)))
	      (list (/ b d) (/ c d))))))



;;; Channels functions interface
(define-wav-function channel (sample chan)
  "Extraxt channel 'chan' from sample"
  (extract-channel sample chan))




;;; Tone functions
(defvar *tone-list* '(:do :do# :re :re# :mi :fa :fa# :sol :sol# :la :la# :si))
(defvar *main-tone-list* '(:do :re :mi :fa :sol :la :si))

(defvar *tone-list-en* '(:c :c# :d :d# :e :f :f# :g :g# :a :a# :b))
(defvar *main-tone-list-en* '(:c :d :e :f :g :a :b))


(define-wav-function tone-list ()
  "Return the tone list"
  *tone-list*)

(define-wav-function main-tone-list ()
  "Return the main tone list"
  *main-tone-list*)


(define-wav-function tone-list-en ()
  "Return the tone list (english)"
  *tone-list-en*)

(define-wav-function main-tone-list-en ()
  "Return the main tone list (english)"
  *main-tone-list-en*)



(define-wav-function tone-up (sample &optional (tone-number 1) (max-num 30) (max-den 30))
  "Tone up the sample by tone-number"
  (pitch sample (find-near (expt (expt 2 1/12) tone-number) max-num max-den)))

(define-wav-function tone-down (sample &optional (tone-number 1) (max-num 30) (max-den 30))
  "Tone down the sample by tone-number"
  (pitch sample (find-near (/ 1 (expt (expt 2 1/12) tone-number)) max-num max-den)))


(defun find-tone (tone)
  (or (position tone *tone-list*)
      (position tone *tone-list-en*)))
  



(define-wav-function tone (sample from to &optional (max-num 30) (max-den 30))
  "Tone the sample from a tone to another
From and to are keywords: do do# re re# mi fa fa# sol sol# la la# si
                     or   c  c#  d  d#  e  f  f#  g   g#   a  a#  b"
  (let ((diff (- (find-tone to) (find-tone from))))
    (cond ((> diff 0) (tone-up sample diff max-num max-den))
	  ((< diff 0) (tone-down sample diff max-num max-den))
	  (t sample))))


(define-wav-function octave-up (sample &optional (max-num 30) (max-den 30))
  "Return a new sample in the next octave with same tone"
  (tone-up sample 12 max-num max-den))


(define-wav-function octave-down (sample &optional (max-num 30) (max-den 30))
  "Return a new sample in the previous octave with same tone"
  (tone-down sample 12 max-num max-den))



(define-wav-function octavify (sample delay &optional (tones (main-tone-list)))
  "Return a new sample with tones in the tones list (it can be the main tone or
all melody tones) separated with delay in seconds."
  (let ((new-sample (tone sample :do (first tones))))
    (loop for i in (rest tones)
	  for d from 1
	  do (setf new-sample (mix new-sample (tone sample :do i) :start (* delay d))))
    new-sample))


(define-wav-function file-tone (base tone)
  "Return the filename of a file based on his base and his tone"
  (format nil "~A-~2,'0D-~(~A~).wav" base (1+ (find-tone tone)) tone))

(define-wav-function symbol-tone (base tone)
  "Return a new symbol based on his base and his tone"
  (intern (string-upcase (format nil "~A-~A" base tone))))


(define-wav-function octavify-to-file (sample filename-base &optional (tones (main-tone-list)))
  "Write sample in all tones in the file filename-base-tone_number-tone.
ex: piano-03-re.wav. Produce one file per tone"
  (loop for i in tones do
	(format t "Writing ~A~%" (file-tone filename-base i))
	(write-sample (file-tone filename-base i)
		      (tone sample :do i))))


(define-wav-macro defparameter-from-tones (sample symbol-base &rest tones)
  "Define some new symbols based on there tones.
Symbol are symbol-base-tone. ex: piano-re"
  `(progn
    ,@(loop for i in tones
	    collect `(defparameter ,(symbol-tone symbol-base i)
		      (tone ,sample :do ,i)))
    (list ,@(loop for i in tones
		  collect `(quote ,(symbol-tone symbol-base i))))))


(define-wav-macro defparameter-from-file (filename-base symbol-base &rest tones)
  "Define some new symbols based on there tones from files produced with
octavify-to-file for example. Symbol are symbol-base-tone. ex: piano-re"
  `(progn
    ,@(loop for i in tones
	    collect `(defparameter ,(symbol-tone symbol-base i)
		      (read-sample ,(file-tone filename-base i))))
    (list ,@(loop for i in tones
		  collect `(quote ,(symbol-tone symbol-base i))))))





