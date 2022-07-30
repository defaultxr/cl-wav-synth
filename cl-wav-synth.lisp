;;; CL-Wav-Synth -  Manipulate WAV files
;;;
;;; Express noises as you think.
;;;
;;; Copyright (C) 2006 Philippe Brochard (hocwp@free.fr)
;;;
;;; #Date#: Wed Oct 25 21:18:51 2006
;;;
;;; **********************************************
;;; The authors grant you the rights to distribute
;;; and use this software as governed by the terms
;;; of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html),
;;; known as the LLGPL.
;;; **********************************************
;;;
;;; USAGE:
;;; -----
;;; start your lisp
;;; CL-USER> (load "/where/is/load.lisp")
;;; CL-USER> (in-package :wav)
;;; WAV> (list-all-test)
;;; WAV> (testN)
;;; ... look at each example to see how they works.
;;;
;;; WAV format description: (hmm, in french, sorry :)
;;; ----------------------
;;;  * Structure generale:  Offset (decimal)  offset (hexa)  nom   longueur (oct.)  description
;;;  
;;;  0 00h rID 4 contient "RIFF"   #52494646
;;;  
;;;  4 04h rLen 4 longueur du fichier
;;;  
;;;  8 08h wID 4 contient "WAVE"   #57415645
;;;  
;;;  * Le Format Chunk: Offset (decimal) offset (hexa) nom longueur (octet) description
;;;  
;;;  12 0Ch fId 4 contient "fmt " ("fmt espace")  #666D7420
;;;  
;;;  16 10h fLen 4 Longueur du Chunck  16 #10
;;;  
;;;  20 14h wFormatTag 2 format (1 = Microsoft Pulse Code Modulation PCM)
;;;  
;;;  22 16h nChannels 2 nombre de canaux (1=mono, 2=stereo)
;;;  
;;;  24 18h nSamplesPerSec 4 frequence d'echantillonage (en Hz)
;;;  
;;;  28 1Ch nAvgBytesPerSec 4 = nChannels * nSamplesPerSec * (nBitsPerSample/8)
;;;  
;;;  32 20h nBlockAlign 2 = nChannels * (nBitsPerSample / 8)
;;;  
;;;  34 22h nBitsPerSample 2 longueur d'un echantillon en bits (8, 16, 24 ou 32)
;;;  
;;;  * Le WAVE Data Chunk: Offset (decimal) offset (hexa) nom longueur (octet) description
;;;  
;;;  36 24h dId 4 contient "data"    #64617461
;;;  
;;;  40 28h dLen 4 longueur du chunck dData (en octets)
;;;  
;;;  44 et plus 2Ch dData dLen les donnees du son echantillonne


(in-package :common-lisp)

(defpackage :cl-wav-synth
  (:use :common-lisp :uni-shell :bezier)
  (:nicknames :wav)
  (:export :time-to-sample
	   :sample-to-time
	   :random-mm
	   :header
	   :sample-p
	   :sample-n-channels
	   :sample-n-samples-per-sec
	   :sample-n-bits-per-sample
	   :sample-time
	   :sample-n-avg-bytes-per-sec
	   :sample-n-block-align
	   :sample-total-byte
	   :sample-last-sample
	   :sample-max-index
	   :sample-max-ampl
	   :sample-name
	   :print-sample
	   :spectre-rl
	   :spectre-im
	   :spectre-time
	   :spectre-n-samples-per-sec
	   :spectre-n-bits-per-sample
	   :data
	   :sample
	   :spectre
	   :t->s
	   :s->t
	   :f->s
	   :find-smin-smax
	   :skip-header
	   :write-fake-header
	   :write-header
	   :read-header
	   :print-header
	   :copy-header
	   :header-equal
	   :make-data
	   :set-data
	   :freq->index
	   :get-ampl
	   :set-ampl
	   :write-sample
	   :read-sample
	   :copy-sample
	   :apply-on-sample
	   :cut-sample
	   :insert-sample
	   :mix-sample
	   :extract-channel
	   :add-channel
	   :fft
	   :time<->freq
	   :sample-make-square
	   :sample-make-line
	   :sample-make-lines
	   :sample-make-sin
	   :sample-make-noise
	   :build-from-freq
	   :read-freq-from-file
	   :sample-view

	   :song-sample
	   :add-tags :del-tags
	   :s-time
	   :s-form
	   :s-pos
	   :s-tags
	   :s-color
	   :s-length

	   :build-song
	   :build-song-in-interval
	   :eval-song-sample-form
	   :list-to-song
	   :with-build-song
	   :with-song
	   :write-song
	   :read-song
	   :*current-song*))

(in-package :cl-wav-synth)

;;; You can edit this variable to your needs
;;; Possibles values are:
;;;   A symbol in: dplay, xmms, snd, audacity, cool-player, totem, bmp, macplay
;;;  or a command line string
(defparameter *default-player* 'dplay
  "The default command to play a sample")

(defvar *spi* (float pi 1f0))

(defvar *current-song* nil)

;;; This is in cl-wav-synth for compilation priority
(defparameter *wav-test-hash* (make-hash-table))


;;; Simples converters

(defun time-to-sample (n-samples-per-sec time)
  "Convert time in sample according to frequence n-samples-per-sec"
  (truncate (* n-samples-per-sec time)))

(defun sample-to-time (n-samples-per-sec n-sample)
  "Convert sample in time according to frequence n-samples-per-sec"
  (/ n-sample n-samples-per-sec))

(defun random-mm (min max)
  "Return a random number between min and max"
  (+ (random (- max min)) min))


;;; Main class

(defclass header ()
  ((n-channels :initarg :n-channels
	       :accessor sample-n-channels)
   (n-samples-per-sec :initarg :n-samples-per-sec
		      :accessor sample-n-samples-per-sec)
   (n-bits-per-sample :initarg :n-bits-per-sample
		      :accessor sample-n-bits-per-sample)
   (time :initarg :time :initform nil :accessor sample-time)
   (n-avg-bytes-per-sec :initform nil :accessor sample-n-avg-bytes-per-sec)
   (n-block-align :initform nil :accessor sample-n-block-align)
   (total-byte :initform nil :accessor sample-total-byte)
   (last-sample :initform nil :accessor sample-last-sample)
   (max-index :initform nil :accessor sample-max-index)
   (max-ampl :initform nil :accessor sample-max-ampl)))

(defclass data ()
  ((data :initarg :data :initform nil :accessor data)))

(defclass sample (header data)
  ((name :initarg :name
	 :initform (format nil "noname-~A.wav" (gensym))
	 :accessor sample-name)))

(defclass spectre (data)
  (;;(rl :initarg :rl :initform 0 :accessor spectre-rl)
   (im :initarg :im :initform 0 :accessor spectre-im)
   (time :initarg :time :initform 0 :accessor spectre-time)
   (n-samples-per-sec :initarg :n-samples-per-sec
		      :accessor spectre-n-samples-per-sec)
   (n-bits-per-sample :initarg :n-bits-per-sample
		      :accessor spectre-n-bits-per-sample)))



;;; Object identification methods
(defgeneric sample-p (object))
(defmethod sample-p ((object sample))
  (declare (ignore object))
  t)
(defmethod sample-p (object)
  (declare (ignore object))
  nil)


;;; Low level functions helper
(defun write-id (stream str)
  (loop for c across str do
	(write-byte (char-code c) stream)))

(defun write-16 (stream n)
  (write-byte (ldb (byte 8 0) n) stream)
  (write-byte (ldb (byte 8 8) n) stream))


(defun write-32 (stream n)
  (write-byte (ldb (byte 8 0) n) stream)
  (write-byte (ldb (byte 8 8) n) stream)
  (write-byte (ldb (byte 8 16) n) stream)
  (write-byte (ldb (byte 8 24) n) stream))


(defun read-id (stream size)
  (let ((answer (loop for i from 1 to size
		      collect (read-byte stream))))
    (map 'string #'code-char answer)))

(defun read-16 (stream)
  (let ((answer (read-byte stream)))
    (when answer
      (setf (ldb (byte 8 8) answer) (read-byte stream)))
    answer))

(defun read-32 (stream)
  (let ((answer (read-byte stream)))
    (when answer
      (setf (ldb (byte 8 8) answer) (read-byte stream))
      (setf (ldb (byte 8 16) answer) (read-byte stream))
      (setf (ldb (byte 8 24) answer) (read-byte stream)))
    answer))



;;; Header functions

(defgeneric t->s (header period))
(defmethod t->s ((header header) period)
  "Convert time period to samples"
  (with-slots (n-samples-per-sec max-index) header
    (let ((s (time-to-sample n-samples-per-sec period)))
      (if (and max-index (>= s max-index))
	  (1- max-index)
	  s))))

(defgeneric s->t (header period))
(defmethod s->t ((header header) index)
  "Convert samples to time period"
  (with-slots (n-samples-per-sec) header
    (sample-to-time n-samples-per-sec index)))



(defgeneric f->s (header freq))
(defmethod f->s ((header header) freq)
  "Convert frequence to samples"
  (truncate (/ (sample-n-samples-per-sec header) freq)))




(defgeneric find-s (header time))
(defmethod find-s ((header header) time)
  "Find samples index from time in a sample"
  (with-slots (n-channels) header
    (if time
	(* (t->s header time) n-channels)
	0)))

(defgeneric find-smin-smax (header start end))
(defmethod find-smin-smax ((header header) start end)
  "Find min and max samples index from min and max time in a sample"
  (with-slots (last-sample n-channels) header
    (values (if start
		(* (t->s header start) n-channels)
		0)
	    (if end
		(* (1+ (t->s header end)) n-channels)
		last-sample))))


(defgeneric skip-header (header))
(defmethod skip-header ((header header))
  (/ 352 (sample-n-bits-per-sample header)))





(defgeneric write-fake-header (filename header))
(defmethod write-fake-header (filename (header header))
  (with-open-file (stream filename :direction :output
			  :if-exists :overwrite :if-does-not-exist :create
			  :element-type '(unsigned-byte 8))
    (write-sequence (make-array 44 :element-type '(unsigned-byte 8)
				:initial-element 0)
		    stream))
  t)

(defgeneric write-header (filename header &key if-exists))
(defmethod write-header (filename (header header) &key (if-exists :overwrite))
  (with-slots (n-samples-per-sec
	       n-channels n-bits-per-sample
	       n-block-align n-avg-bytes-per-sec
	       total-byte time) header
    (with-open-file (stream filename :direction :output
			    :if-exists if-exists
			    :if-does-not-exist :create
			    :element-type '(unsigned-byte 8))
      (write-id stream "RIFF")
      (write-32 stream (+ 44 total-byte))
      (write-id stream "WAVE")
      (write-id stream "fmt ")
      (write-32 stream 16)
      (write-16 stream 1)
      (write-16 stream n-channels)
      (write-32 stream n-samples-per-sec)
      (write-32 stream n-avg-bytes-per-sec)
      (write-16 stream n-block-align)
      (write-16 stream n-bits-per-sample)
      (write-id stream "data")
      (write-32 stream total-byte)))
  t)

(defgeneric read-header (filename header))
(defmethod read-header (filename (header header))
  "Read wav header info. See http://www.sonicspot.com/guide/wavefiles.html"
  (labels ((expected (read-str orig-str)
	     (assert (string= read-str orig-str) ()
		     "error reading header: ~S is not a wav file. Expected ~A Got ~A"
		     filename orig-str read-str)))
    (with-slots (n-samples-per-sec
		 n-channels n-bits-per-sample
		 n-block-align n-avg-bytes-per-sec
		 total-byte) header
      (with-open-file (stream filename :direction :input
			      :element-type '(unsigned-byte 8))
	(expected (read-id stream 4) "RIFF")
	(read-32 stream)
	(expected (read-id stream  4) "WAVE")
        (loop
         (let* ((next-header (read-id stream 4))
                (bytes (read-32 stream)))
           (cond ((string= next-header "fmt ")
                  (read-16 stream) ;; compression code
                  (setf n-channels (read-16 stream)) 
                  (setf n-samples-per-sec (read-32 stream))
                  (setf n-avg-bytes-per-sec (read-32 stream))
                  (setf n-block-align (read-16 stream))
                  (setf n-bits-per-sample (read-16 stream))
                  ;; possible extra format bytes
                  (dotimes (i (- bytes 16)) (read-byte stream)))
                 ((string= next-header "data")
                  (setf total-byte bytes)
                  (return))
                 (t
                  ;; There're a lot of headers that we don't
                  ;; care. For instance, bext minf elmo, etc
                  (dotimes (i bytes) (read-byte stream)))))))))
  header)

(defgeneric print-header (header &optional comment))
(defmethod print-header ((header header) &optional (comment ""))
  (with-slots (n-samples-per-sec
	       n-channels n-bits-per-sample
	       n-block-align n-avg-bytes-per-sec
	       total-byte last-sample max-index
	       time max-ampl) header
    (format t "~&Header: [~A]
  n-samples-per-sec   = ~A Hz
  n-channels          = ~A chan
  n-bits-per-sample   = ~A bits/sample
  n-block-align       = ~A bytes
  n-avg-bytes-per-sec = ~A bytes/s
  total-byte          = ~A bytes
  last-sample         = ~A (data sample length)
  max-index           = ~A (one channel length)
  time                = ~,2F s
  max-ampl            = ~A~%"
	    comment
	    n-samples-per-sec
	    n-channels n-bits-per-sample
	    n-block-align n-avg-bytes-per-sec
	    total-byte last-sample max-index
	    time max-ampl)))



(defgeneric set-sample-info (arg))
(defmethod set-sample-info ((header header))
  (with-slots (n-samples-per-sec
	       n-channels n-bits-per-sample
	       n-block-align n-avg-bytes-per-sec max-ampl) header
    (setf n-block-align (* n-channels (/ n-bits-per-sample 8)))
    (setf n-avg-bytes-per-sec (* n-samples-per-sec n-block-align))
    (setf max-ampl (1- (expt 2 (1- n-bits-per-sample))))))

(defgeneric set-total-byte-from-time (arg))
(defmethod set-total-byte-from-time ((header header))
  (with-slots (n-samples-per-sec
	       total-byte n-block-align time) header
    (when (numberp time)
      (setf total-byte
	    (* n-block-align (time-to-sample n-samples-per-sec time))))))

(defgeneric set-total-byte-from-data (arg1 &optional arg2))
(defmethod set-total-byte-from-data ((header header) &optional (data-length 1))
  (with-slots (n-bits-per-sample total-byte) header
    (setf total-byte
	  (* (/ n-bits-per-sample 8) data-length))))


(defgeneric set-last-sample (arg))
(defmethod set-last-sample ((header header))
  (with-slots (n-bits-per-sample
	       n-samples-per-sec
	       last-sample total-byte max-index n-channels time) header
    (setf last-sample (if (numberp total-byte)
			  (/ total-byte (/ n-bits-per-sample 8))
			  0)
	  max-index (/ last-sample n-channels)
	  time (float (sample-to-time n-samples-per-sec max-index)))))

(defgeneric copy-header (header))
(defmethod copy-header ((header header))
  (with-slots (n-channels n-samples-per-sec n-bits-per-sample) header
    (make-instance 'header :n-channels n-channels
		   :n-samples-per-sec n-samples-per-sec
		   :n-bits-per-sample n-bits-per-sample)))

(defgeneric header-equal (header1 header2))
(defmethod header-equal ((header1 header) (header2 header))
  "Compare only significant slot from two headers"
  (and (equal (sample-n-channels header1)
	      (sample-n-channels header2))
       (equal (sample-n-bits-per-sample header1)
	      (sample-n-bits-per-sample header2))
       (equal (sample-n-samples-per-sec header1)
	      (sample-n-samples-per-sec header2))))



;;; Data functions

(defun make-data (size n-bits-per-sample)
  (make-array size
	      :element-type `(signed-byte ,n-bits-per-sample)
	      :initial-element 0))

(defgeneric set-data (data &optional last-sample total-byte n-bits-per-sample))
(defmethod set-data ((data data)
		     &optional (last-sample 1)
		     (total-byte 1) (n-bits-per-sample 1))
  (with-slots (data) data
    (when (numberp total-byte)
      (setf data (make-data last-sample n-bits-per-sample)))))


;;; Spectre functions
(defgeneric freq->index (spectre freq))
(defmethod  freq->index ((spectre spectre) freq)
  (floor (* freq (spectre-time spectre))))



;;; Sample functions
(defmacro get-ampl (sample chan index)
  `(aref (data ,sample)
    (+ (* ,index (sample-n-channels ,sample)) ,chan)))

(defgeneric set-ampl (sample fun chan index ampl))
(defmethod set-ampl ((sample sample) fun chan index ampl)
  (with-slots (max-ampl) sample
    (let ((val (truncate (funcall fun
				  (get-ampl sample chan index)
				  ampl))))
      (setf (get-ampl sample chan index)
	    (cond ((>= val max-ampl) max-ampl)
		  ((<= val (- max-ampl)) (- max-ampl))
		  (t val))))))



(defmethod set-total-byte-from-data ((sample sample) &optional ignored)
  (declare (ignore ignored))
  (with-slots (n-bits-per-sample data total-byte) sample
    (setf total-byte
	  (* (/ n-bits-per-sample 8) (length data)))))


(defmethod set-data ((sample sample) &optional ignored1 ignored2 ignored3)
  (declare (ignore ignored1 ignored2 ignored3))
  (with-slots (total-byte
	       n-bits-per-sample data last-sample) sample
    (when (numberp total-byte)
      (setf data (make-data last-sample n-bits-per-sample)))))



(defgeneric initialize (arg))
(defmethod initialize ((sample sample))
  (set-sample-info sample)
  (set-total-byte-from-time sample)
  (set-last-sample sample)
  (set-data sample))
  

(defmethod initialize-instance :after ((sample sample) &key)
  (initialize sample))

(defgeneric print-sample (sample))
(defmethod print-sample ((sample sample))
  (format t "Sample: ~A~%" (sample-name sample))
  (print-header sample))

(defun pad-with-zero (stream pos element-type)
  (let ((len (file-length stream)))
    (when (> pos len)
      (let ((padsize (- pos len)))
	(file-position stream len)
	(write-sequence (make-array padsize :initial-element 0
				    :element-type element-type) stream)))))



(defun swap-indan-8-big->little (data un-data)
  (let ((v 0))
    (dotimes (i (length data))
      (setf v (aref data i))
      (setf (aref un-data i)
	    (if (>= v 0) v (logxor (1- (- v)) #xFF))))))

(defun swap-indan-16-big->little (data un-data)
  (let ((v 0))
    (dotimes (i (length data))
      (setf v (aref data i))
      (rotatef (ldb (byte 8 0) v) (ldb (byte 8 8) v))
      (setf (aref un-data i)
	    (if (>= v 0) v (logxor (1- (- v)) #xFFFF))))))

(defun swap-indan-32-big->little (data un-data)
  (let ((v 0))
    (dotimes (i (length data))
      (setf v (aref data i))
      (rotatef (ldb (byte 8 0) v) (ldb (byte 8 24) v))
      (rotatef (ldb (byte 8 8) v) (ldb (byte 8 16) v))
      (setf (aref un-data i)
	    (if (>= v 0) v (logxor (1- (- v)) #xFFFFFFFF))))))



(defun swap-indan-8-little->big (un-data data)
  (let ((v 0))
    (dotimes (i (length un-data))
      (setf v (aref un-data i))
      (setf (aref data i)
	    (if (zerop (logand v #x80))
		(logand v #x7F)
		(- (1+ (logxor v #xFF))))))))


(defun swap-indan-16-little->big (un-data data)
  (let ((v 0))
    (dotimes (i (length un-data))
      (setf v (aref un-data i))
      (rotatef (ldb (byte 8 0) v) (ldb (byte 8 8) v))
      (setf (aref data i)
	    (if (zerop (logand v #x8000))
		(logand v #x7FFF)
		(- (1+ (logxor v #xFFFF))))))))

(defun swap-indan-32-little->big (un-data data)
  (let ((v 0))
    (dotimes (i (length un-data))
      (setf v (aref un-data i))
      (rotatef (ldb (byte 8 0) v) (ldb (byte 8 24) v))
      (rotatef (ldb (byte 8 8) v) (ldb (byte 8 16) v))
      (setf (aref data i)
	    (if (zerop (logand v #x80000000))
		(logand v #x7FFFFFFF)
		(- (1+ (logxor v #xFFFFFFFF))))))))


(defgeneric write-sample (filename sample &key start))

(defmethod write-sample (filename (sample (eql nil)) &key start)
  (declare (ignore filename sample start))
  nil)

#-PPC
(defmethod write-sample (filename (sample sample) &key start)
  (labels ((l-write ()
	     (with-slots (n-bits-per-sample data) sample
	       (write-header filename sample :if-exists :supersede)
	       (with-open-file (stream filename :direction :output
				       :if-exists :overwrite
				       :if-does-not-exist :create
				       :element-type
				       `(signed-byte ,n-bits-per-sample))
		 (file-position stream (skip-header sample))
		 (write-sequence data stream))))
	   (l-merge ()
	     (let ((header (copy-header sample)))
	       (when (probe-file filename)
		 (read-header filename header)
		 (assert (header-equal sample header) ()
			 "error writing wav: ~S is in a wrong format"
			 filename))
	       (set-sample-info header)
	       (write-fake-header filename header)
	       (with-open-file (stream filename :direction :output
				       :if-exists :overwrite
				       :if-does-not-exist :create
				       :element-type
				       `(signed-byte
					 ,(sample-n-bits-per-sample header)))
		 (let ((pos-start (+ (skip-header header)
				     (* (t->s header start) 
					(sample-n-channels header)))))
		   (pad-with-zero stream pos-start 
				  `(signed-byte 
				    ,(sample-n-bits-per-sample header)))
		   (file-position stream pos-start)
		   (write-sequence (data sample) stream)
		   (file-position stream 0)
		   (set-total-byte-from-data header (- (file-length stream)
						       (skip-header header)))
		   (set-last-sample header))
		 (write-header filename header)
		 (read-header filename header)))))
    (if start (l-merge) (l-write))
    t))



#+PPC
(defmethod write-sample (filename (sample sample) &key start)
  (labels ((write-un-data (stream data n-bits-per-sample)
	     (let ((un-data (make-array (length data) :element-type
					`(unsigned-byte ,n-bits-per-sample))))
		   (case n-bits-per-sample
		     (8 (swap-indan-8-big->little data un-data))
		     (16 (swap-indan-16-big->little data un-data))
		     (32 (swap-indan-32-big->little data un-data)))
		   (write-sequence un-data stream)))
	   (l-write ()
	     (with-slots (n-bits-per-sample data) sample
	       (write-header filename sample :if-exists :supersede)
	       (with-open-file (stream filename :direction :output
				       :if-exists :overwrite
				       :if-does-not-exist :create
				       :element-type
				       `(unsigned-byte ,n-bits-per-sample))
		 (file-position stream (skip-header sample))
		 (write-un-data stream data n-bits-per-sample))))
	   (l-merge ()
	     (let ((header (copy-header sample)))
	       (when (probe-file filename)
		 (read-header filename header)
		 (assert (header-equal sample header) ()
			 "error writing wav: ~S is in a wrong format"
			 filename))
	       (set-sample-info header)
	       (write-fake-header filename header)
	       (with-open-file (stream filename :direction :output
				       :if-exists :overwrite
				       :if-does-not-exist :create
				       :element-type
				       `(unsigned-byte
					 ,(sample-n-bits-per-sample header)))
		 (let ((pos-start (+ (skip-header header)
				     (* (t->s header start) 
					(sample-n-channels header)))))
		   (pad-with-zero stream pos-start 
				  `(signed-byte 
				    ,(sample-n-bits-per-sample header)))
		   (file-position stream pos-start)
		   (write-un-data stream (data sample)
				  (sample-n-bits-per-sample sample))
		   (file-position stream 0)
		   (set-total-byte-from-data header (- (file-length stream)
						       (skip-header header)))
		   (set-last-sample header))
		 (write-header filename header)
		 (read-header filename header)))))
    (if start (l-merge) (l-write))
    t))



#-PPC
(defun read-sample (filename &key start end name)
  (let ((sample (make-instance 'sample :n-channels 1
			       :n-samples-per-sec 22050
			       :n-bits-per-sample 16
			       :name (or name (file-namestring filename)))))
    (with-slots (n-bits-per-sample data) sample
      (read-header filename sample)
      (set-sample-info sample)
      (set-last-sample sample)
      (multiple-value-bind (smin smax)
	  (find-smin-smax sample start end)
	(setf data (make-data (- smax smin) n-bits-per-sample))
	(with-open-file (stream filename :direction :input
				:element-type
				`(signed-byte ,n-bits-per-sample))
	  (file-position stream (+ (skip-header sample) smin))
	  (read-sequence data stream)))
      (set-total-byte-from-data sample)
      (set-last-sample sample)
      sample)))

#+PPC
(defun read-sample (filename &key start end name)
  (let ((sample (make-instance 'sample :n-channels 1
			       :n-samples-per-sec 22050
			       :n-bits-per-sample 16
			       :name (or name (file-namestring filename)))))
    (with-slots (n-bits-per-sample data) sample
      (read-header filename sample)
      (set-sample-info sample)
      (set-last-sample sample)
      (multiple-value-bind (smin smax)
	  (find-smin-smax sample start end)
	(setf data (make-data (- smax smin) n-bits-per-sample))
	(let ((un-data (make-array (length data) :element-type
				   `(unsigned-byte ,n-bits-per-sample))))
	  (with-open-file (stream filename :direction :input
				  :element-type
				  `(unsigned-byte ,n-bits-per-sample))
	    (file-position stream (+ (skip-header sample) smin))
	    (read-sequence un-data stream))
	  (case n-bits-per-sample
	    (8 (swap-indan-8-little->big un-data data))
	    (16 (swap-indan-16-little->big un-data data))
	    (32 (swap-indan-32-little->big un-data data)))))
      (set-total-byte-from-data sample)
      (set-last-sample sample)
      sample)))


(defgeneric copy-sample (sample &key start end name))
(defmethod copy-sample ((sample sample) &key start end name)
  (with-slots (n-samples-per-sec
	       n-bits-per-sample n-channels data
	       (sample-name name)) sample
    (let ((new-sample (make-instance 'sample
				     :n-channels n-channels
				     :n-samples-per-sec n-samples-per-sec
				     :n-bits-per-sample n-bits-per-sample
				     :name (or name sample-name))))
      (set-sample-info new-sample)
      (multiple-value-bind (smin smax)
	  (find-smin-smax sample start end)
	(setf (data new-sample) (subseq data smin smax)))
      (set-total-byte-from-data new-sample)
      (set-last-sample new-sample)
      new-sample)))


(defgeneric apply-on-sample (sample fun &key args start end))
(defmethod apply-on-sample (sample fun &key args start end)
  (with-slots (n-samples-per-sec
	       n-bits-per-sample n-channels data
	       (sample-name name)) sample
    (let ((new-sample (make-instance 'sample
				     :n-channels n-channels
				     :n-samples-per-sec n-samples-per-sec
				     :n-bits-per-sample n-bits-per-sample))
	  val)
      (set-sample-info new-sample)
      (multiple-value-bind (smin smax)
	  (find-smin-smax sample start end)
	(with-slots ((new-data data) max-ampl) new-sample
	  (setf new-data (copy-seq data))
	  (loop for i from smin below smax do
		(setf val (truncate (apply fun i
					   (aref data i)
					   args)))
		(setf (aref new-data i)
		      (cond ((>= val max-ampl) max-ampl)
			    ((<= val (- max-ampl)) (- max-ampl))
			    (t val))))))
      (set-total-byte-from-data new-sample)
      (set-last-sample new-sample)
      new-sample)))





(defun cut-sample (sample start end &key name)
  "Return a new sample without the cutted part"
  (with-slots (n-samples-per-sec
	       n-bits-per-sample n-channels data
	       (sample-name name)) sample
    (let ((new-sample (make-instance 'sample
				     :n-channels n-channels
				     :n-samples-per-sec n-samples-per-sec
				     :n-bits-per-sample n-bits-per-sample
				     :name (or name sample-name))))
      (set-sample-info new-sample)
      (multiple-value-bind (smin smax)
	  (find-smin-smax sample start end)
	(setf (data new-sample) (concatenate 'vector
					     (subseq data 0 smin)
					     (subseq data smax))))
      (set-total-byte-from-data new-sample)
      (set-last-sample new-sample)
      new-sample)))


(defun insert-sample (sample sample2 start &key name)
  "Return a new sample with sample2 inserted at start seconds"
  (with-slots (n-samples-per-sec
	       n-bits-per-sample n-channels data max-index
	       (sample-name name)) sample
    (assert (header-equal sample sample2)
	    ()
	    "error samples must have the same format")
    (let ((new-sample (make-instance 'sample
				     :n-channels n-channels
				     :n-samples-per-sec n-samples-per-sec
				     :n-bits-per-sample n-bits-per-sample
				     :name (or name sample-name)))
	  (ind (time-to-sample n-samples-per-sec start)))
      (set-sample-info new-sample)
      (setf (data new-sample)
	    (if (< ind max-index)
		(concatenate 'vector
			     (subseq data 0 ind)
			     (data sample2)
			     (subseq data ind))
		(concatenate 'vector
			     data
			     (make-array (- ind max-index) :initial-element 0
					 :element-type `(signed-byte ,n-bits-per-sample))
			     (data sample2))))
      (set-total-byte-from-data new-sample)
      (set-last-sample new-sample)
      new-sample)))







(defun calc-new-len (len1 len2 start)
  "Return the new length of a sample with 1 channel
        <--- len 1 --->
             <----- len 2 ----->
        ^    |_start           ^
        |----- new len --------|"
  (let ((rest (- (+ len2 start) len1)))
    (+ len1 (if (> rest 0) rest 0))))

(defgeneric mix-sample (sample1 sample2 fun &key args start))
(defmethod mix-sample ((sample1 sample) (sample2 sample) fun
		       &key args (start 0))
  (with-slots ((n-channels-1 n-channels)
	       (n-bits-per-sample-1 n-bits-per-sample)
	       (n-samples-per-sec-1 n-samples-per-sec)
	       (data-1 data)) sample1
    (with-slots ((n-channels-2 n-channels)
		 (n-bits-per-sample-2 n-bits-per-sample)
		 (n-samples-per-sec-2 n-samples-per-sec)
		 (data-2 data)) sample2
      (assert (header-equal sample1 sample2)
	      ()
	      "error sample and new channel must have the same format")
      (assert (>= start 0) ()
	      "error start must be a null or positive time in seconds")
      (let* ((len1 (length data-1))
	     (len2 (length data-2))
	     (s-start (* (time-to-sample n-samples-per-sec-2 start) n-channels-2))
	     (new-len (* (calc-new-len (/ len1 n-channels-1)
				       (/ len2 n-channels-2)
				       (/ s-start n-channels-2))
			 n-channels-1))
	     (new-sample (make-instance 'sample :n-channels n-channels-1
					:n-bits-per-sample n-bits-per-sample-1
					:n-samples-per-sec n-samples-per-sec-1
					:data (make-data new-len
							 n-bits-per-sample-1)))
	     val)
	(set-sample-info new-sample)
	(set-total-byte-from-data new-sample)
	(set-last-sample new-sample)
	(with-slots (data max-ampl) new-sample
	  (dotimes (i new-len)
	    (setf val (truncate (apply fun i
				       (if (< i len1) (aref data-1 i) 0)
				       (if (< s-start i (+ s-start len2))
					   (aref data-2 (- i s-start))
					   0)
				       args)))
	    (setf (aref data i)
		  (cond ((>= val max-ampl) max-ampl)
			((<= val (- max-ampl)) (- max-ampl))
			(t val)))))
	new-sample))))
  

(defmethod mix-sample ((filename string) (sample2 sample) fun
		       &key args (start 0))
  (let ((sample1 (read-sample filename
			      :start start
			      :end (+ start (sample-time sample2)))))
    (write-sample filename 
		  (mix-sample sample1 sample2 fun :args args)
		  :start start)))


(defgeneric extract-channel (sample chan))
(defmethod extract-channel ((sample sample) chan)
  (with-slots (n-channels
	       n-bits-per-sample n-samples-per-sec data) sample
    (assert (<= 0 chan (1- n-channels)) ()
	    "error new channel can't be ~A (chan=[0..~A])"
	    chan (1- n-channels))
    (let* ((len (/ (length data) n-channels))
	   (new-sample (make-instance 'sample :n-channels 1
				      :n-bits-per-sample n-bits-per-sample
				      :n-samples-per-sec n-samples-per-sec
				      :data (make-data len
						       n-bits-per-sample))))
      (set-sample-info new-sample)
      (set-total-byte-from-data new-sample)
      (set-last-sample new-sample)
      (with-slots ((new-data data)) new-sample
	(dotimes (i len)
	  (setf (aref new-data i)
		(aref data (+ (* i n-channels) chan)))))
      new-sample)))


;;; data:   |1 2|1 2|1 2|1 2|1 2|   2 channels
;;; New-chan-data: |3|3|3|
;;; => new-data:   |1 2 3|1 2 3|1 2 3|1 2 0|1 2 0| with start=0
;;; => new-data:   |1 2 0|1 2 3|1 2 3|1 2 3|1 2 0| with start=1
;;; => new-data:   |1 2 0|1 2 0|1 2 0|1 2 3|1 2 3|1 2 3| with start=3
(defgeneric add-channel (sample chan new-chan &key start))
(defmethod add-channel ((sample sample) chan (new-chan sample) &key (start 0))
  (with-slots ((n-channels-1 n-channels)
	       (n-bits-per-sample-1 n-bits-per-sample)
	       (n-samples-per-sec-1 n-samples-per-sec)
	       (data-1 data)) sample
    (with-slots ((n-channels-2 n-channels)
		 (n-bits-per-sample-2 n-bits-per-sample)
		 (n-samples-per-sec-2 n-samples-per-sec)
		 (data-2 data)) new-chan
      (assert (= n-channels-2 1) ()
	      "error new channel must be a 1 channel sample")
      (assert (<= 0 chan n-channels-1) ()
	      "error new channel can't be put in channel ~A (chan=[0..~A])"
	      chan n-channels-1)
      (assert (and (= n-bits-per-sample-1 n-bits-per-sample-2)
		   (= n-samples-per-sec-1 n-samples-per-sec-2))
	      ()
	      "error sample and new channel must have the same format")
      (assert (>= start 0) ()
	      "error start must be a null or positive time in seconds")
      (let* ((len1 (/ (length data-1) n-channels-1))
	     (len2 (length data-2))
	     (s-start (time-to-sample n-samples-per-sec-2 start))
	     (new-n-chan (1+ n-channels-1))
	     (new-sample (make-instance 'sample :n-channels new-n-chan
					:n-bits-per-sample n-bits-per-sample-1
					:n-samples-per-sec n-samples-per-sec-1
					:data
					(make-data (* (calc-new-len
						       len1 len2 s-start)
						      new-n-chan)
						   n-bits-per-sample-1))))
	(set-sample-info new-sample)
	(set-total-byte-from-data new-sample)
	(set-last-sample new-sample)
	(with-slots (data) new-sample
	  (dotimes (i len1)
	    (dotimes (j n-channels-1)
	      (setf (aref data (+ (* i new-n-chan)
				  (if (>= j chan) (1+ j) j)))
		    (aref data-1 (+ (* i n-channels-1) j)))))
	  (dotimes (i len2)
	    (setf (aref data (+ (* (+ i s-start) new-n-chan) chan))
		  (aref data-2 i)))
	  new-sample)))))


;;; Time to Freq functions

(defun fft (rl im dir)
  (let* ((len (length rl))
	 (n (if (oddp len) (1- len) len))
	 (n/2 (/ n 2))
	 (imh (truncate (/ (log (1+ n)) (log 2.0)))))
    ;; bits inversion
    (loop for i below n
	  with j = 0
	  with m do
	  (when (> j i)
	    (rotatef (aref rl j) (aref rl i))
	    (rotatef (aref im j) (aref im i)))
	  (setf m n/2)
	  (loop while (and (>= m 2) (>= j m)) do
		(setf j (- j m)
		      m (truncate (/ m 2))))
	  (setf j (+ j m)))
    ;; FFT calculation
    (loop for lg below imh
	  with m = 2
	  with ldm = 1
	  with mh = n/2
	  with angle = (* pi dir)
	  with i    with j   with u
	  with ur   with ui
	  with vr   with vi
	  with c    with s do
	  (setf c (cos angle)
		s (sin angle)
		ur 1.0
		ui 0.0)
	  (loop for i2 below ldm do
		(setf i i2
		      j (+ i2 ldm))
		(loop for j2 below mh do
		      (setf vr (- (* ur (aref rl j)) (* ui (aref im j)))
			    vi (+ (* ur (aref im j)) (* ui (aref rl j)))
			    (aref rl j) (- (aref rl i) vr)
			    (aref im j) (- (aref im i) vi))
		      (incf (aref rl i) vr)
		      (incf (aref im i) vi)
		      (incf i m)
		      (incf j m))
		(setf u ur
		      ur (- (* ur c) (* ui s))
		      ui (+ (* ui c) (* u s))))
	  (setf mh (truncate (/ mh 2))
		ldm m
		angle (* angle 0.5)
		m (* m 2)))
    (when (= dir 1)
      (dotimes (i len)
	(setf (aref rl i) (/ (aref rl i) n))
	(setf (aref im i) (/ (aref im i) n))))
    (values rl im)))
	   
(defgeneric time<->freq (object))
(defmethod time<->freq ((sample sample))
  (with-slots (n-samples-per-sec
	       data n-bits-per-sample n-channels time) sample
    (assert (= n-channels 1) ()
	    "error sample must have exactly one channel and not ~A"
	    n-channels)
    (let ((spectre (make-instance 'spectre :time time
				  :n-samples-per-sec n-samples-per-sec
				  :n-bits-per-sample n-bits-per-sample
				  :data (make-array (length data)
						    :initial-contents
						    (copy-seq data))
				  :im (make-array (length data)
						  :initial-element 0))))
      (fft (data spectre) (spectre-im spectre) 1)
      spectre)))

(defmethod time<->freq ((spectre spectre))
  (with-slots (n-samples-per-sec
	       n-bits-per-sample data im time) spectre
    (print 'debut)
    (let* ((len (length data))
	   (sample (make-instance 'sample :n-channels 1
				  :n-samples-per-sec n-samples-per-sec
				  :n-bits-per-sample n-bits-per-sample
				  :time time
				  :data (make-data (length data)
						   n-bits-per-sample)))
	   (rl (copy-seq data))
	   (im (copy-seq im)))
      (set-sample-info sample)
      (format t "Max spectre=~A~%"
	      (loop for i across rl maximize i))
      (fft rl im -1)
      (print 'ici)
      (format t "Max spectre=~A~%"
	      (loop for i across rl maximize i))
      (with-slots (max-ampl) sample
	(let (val)
	  (dotimes (i (length rl))
	    (setf val (truncate (* (aref rl i) len)))
	    (setf (aref (data sample) i)
		  (cond ((>= val max-ampl) max-ampl)
			((<= val (- max-ampl)) (- max-ampl))
			(t val))))))
      (format t "Max spectre=~A~%"
	      (loop for i across (data sample) maximize i))
      (set-total-byte-from-data sample)
      (set-last-sample sample)      
      sample)))


;;; Generators

(defgeneric sample-make-square (sample fun chan start end freq
				       minampl maxampl))
(defmethod sample-make-square ((sample sample) fun chan start end freq
			       minampl maxampl)
  "Add a square on sample"
  (let* ((smin (t->s sample start))
	 (smax (min (t->s sample end)
		    (- (sample-max-index sample) 2)))
	 (sfreq (f->s sample freq))
	 (sfreq/2 (/ sfreq 2)))
    (loop for i from smin to smax do
	  (set-ampl sample fun chan i
		    (if (> sfreq/2 (mod (- i smin) sfreq))
			maxampl minampl)))))

(defgeneric sample-make-line (sample fun chan start end freq minampl maxampl))
(defmethod sample-make-line ((sample sample) fun chan start end freq
			     minampl maxampl)
  "Add a line on sample"
  (let* ((smin (t->s sample start))
	 (smax (min (t->s sample end)
		    (- (sample-max-index sample) 2)))
	 (sfreq (f->s sample freq))
	 (dy (/ (- maxampl minampl) sfreq)))
    (loop for i from smin to smax do
	  (set-ampl sample fun chan i
		    (+ minampl (* (mod (- i smin) sfreq) dy))))))


(defgeneric sample-make-line* (sample fun chan start end freq ampls))
(defmethod sample-make-line* ((sample sample) fun chan start end freq ampls)
  "Add lines on sample. Ampls is a list like '((val1 ampl1) (val2 ampl2)...)
   where 0 <= val <= 1"
  (let* ((smin (t->s sample start))
	 (smax (t->s sample end))
	 (sfreq (f->s sample freq))
	 (points (mapcar #'(lambda (x)
			     (list (* (first x) sfreq) (second x)))
			 ampls)))
    (labels ((line (mins maxs mina maxa)
	       (let ((dy (/ (- maxa mina) (- maxs mins))))
		 (loop for i from smin to smax do
		       (when (<= mins (mod (- i smin) sfreq) maxs)
			 (set-ampl sample fun chan i
				   (+ mina (* (mod (- i smin mins) sfreq)
					      dy))))))))
      (loop for last = (first points) then p
	    for p in (cdr points) do
	    (line (first last) (first p) (second last) (second p))))))


(defgeneric sample-make-sin (sample fun chan start end freq ampl sin-fun
				    &optional phase))
(defmethod sample-make-sin ((sample sample) fun chan start end freq ampl
			    sin-fun &optional (phase 0))
  "Add a sinus on sample"
  (let* ((smin (t->s sample start))
	 (smax (min (t->s sample end)
		    (- (sample-max-index sample) 2)))
	 (sfreq (f->s sample freq)))
    (loop for i from smin to smax do
	  (set-ampl sample fun chan i
		    (* ampl
		       (funcall sin-fun
				(+ (* 2 *spi* (/ (- i smin) sfreq)) phase)))))))

(defgeneric sample-make-noise (sample fun chan start end freq minampl maxampl))
(defmethod sample-make-noise ((sample sample) fun chan start end freq
			      minampl maxampl)
  "Add a noise on sample"
  (let* ((smin (t->s sample start))
	 (smax (min (t->s sample end)
		    (- (sample-max-index sample) 2)))
	 (sfreq (f->s sample freq)))
    (loop for i from smin to smax
	  with val = 0 do
	  (when (zerop (mod (- i smin) sfreq))
	    (setf val (random-mm minampl maxampl)))
	  (set-ampl sample fun chan i val))))




(defun build-from-freq (freqs &key (n-samples-per-sec 22050)
			(n-bits-per-sample 16) (time 1))
  "Build a new sample based on frequences and amplitudes
   freqs is a list like this '((freq1 ampl1 phase1) (freq2 ampl2 phase2) ...)"
  (labels ((set-fun (data new)
	     (declare (ignore data))
	     new))
    (let ((sample (make-instance 'sample :n-channels 1
				 :n-samples-per-sec n-samples-per-sec
				 :n-bits-per-sample n-bits-per-sample
				 :time time)))
      (with-slots (max-ampl) sample
	(sample-make-sin sample #'set-fun 0 0 time (first (first freqs))
			 (* max-ampl (second (first freqs))) #'sin)
	(dolist (freq (rest freqs))
	  (when (plusp (second freq))
	    (sample-make-sin sample #'+ 0 0 time (first freq)
			     (* max-ampl (second freq)) #'sin
			     (third freq)))))
      sample)))

  
(defun read-freq-from-file (filename base-freq)
  "Read frequences and phases from a file :
    - first column gain in decibels
    - second column phase in radian"
  (with-open-file (stream filename :direction :input)
    (loop for line = (read-line stream nil :eof)
	  for i from 1
	  while (not (eql line :eof))
	  collect (multiple-value-bind (gain pos)
		      (read-from-string line)
		    (list (* base-freq i)
			  (expt 10 (/ gain 20))
			  (read-from-string (subseq line pos)))))))



;;; Viewers (with gnuplot)
(defgeneric sample-view (object &key min max fun))


(defmacro with-view ((stream min max) &body body)
  (let ((dataname (gensym))
	(filename (gensym)))
    `(let ((,dataname (format nil "data-~A.log" (gensym)))
	   (,filename (format nil "gnuplot-~A.gnuplot" (gensym))))
      (with-open-file (,stream ,filename :direction :output
		       :if-exists :supersede
		       :if-does-not-exist :create)
	(format ,stream "set mouse~%")
	(format ,stream "plot [~A:~A] ~S with lines~%" ,min ,max ,dataname)
	(format ,stream "pause mouse~%"))
      (with-open-file (,stream ,dataname :direction :output
		       :if-exists :supersede
		       :if-does-not-exist :create)
	(progn
	  ,@body))
      (ushell "gnuplot" ,filename)
      (delete-file ,dataname)
      (delete-file ,filename))))


(defmethod sample-view ((spectre spectre) &key (min 0) (max 1000) (fun #'abs))
  (with-view (stream min max)
    (loop for i from (freq->index spectre min)
	  to (freq->index spectre max) do
	  (format stream "~A  ~A~%"
		  (/ i (spectre-time spectre))
		  (funcall fun (complex (aref (data spectre) i)
					(aref (spectre-im spectre) i)))))))

(defmethod sample-view ((sample sample) &key (min 0) (max 1000) (fun 0))
  (with-view (stream min max)
    (with-slots (max-ampl) sample
      (loop for i from (t->s sample min) to (t->s sample max) do
	    (format stream "~A  ~A~%"
		    (float (s->t sample i))
		    (get-ampl sample fun i))))))




;;; Song builder

(defclass song-sample ()
  ((time :initarg :time :initform 0 :accessor s-time)
   (form :initarg :form :initform nil :accessor s-form)
   (pos :initarg :pos :initform 0 :accessor s-pos)
   (tags :initarg :tags :initform nil :accessor s-tags)
   (color :initarg :color :initform #x00FF00 :accessor s-color)
   (length :initarg :length :initform 0.2 :accessor s-length)))


(defun listify (elem)
  "Build a list from elem"
  (if (or (null elem) (consp elem))
      elem
      (list elem)))
      

(defgeneric add-tags (song-sample tag-elem))
(defgeneric del-tags (song-sample tag-elem))

(defmethod add-tags ((song-sample song-sample) tag-elem)
  "Add all tags in tag-elem to song-sample
tag-elem can be a tag or a list of tags"
  (setf (s-tags song-sample)
	(append (listify (s-tags song-sample))
		(listify tag-elem))))

(defmethod del-tags ((song-sample song-sample) tag-elem)
  "Delete all tags in song-sample from tag-elem
tag-elem can be a tag or a list of tags"
  (setf (s-tags song-sample)
	(if (consp tag-elem)
	    (set-difference (s-tags song-sample) tag-elem)
	    (remove tag-elem (s-tags song-sample)))))


(defun copy-song-sample (song-sample &key time form pos tags color length)
  "Copy a song sample"
  (make-instance 'song-sample :time (or time (s-time song-sample))
		 :form (or form (s-form song-sample))
		 :pos (or pos (s-pos song-sample))
		 :tags (or tags (s-tags song-sample))
		 :color (or color (s-color song-sample))
		 :length (or length (s-length song-sample))))



(defun eval-song-sample-form (song-sample)
  "Eval a song sample form, return the produced sample and set
the length to the duration of sample"
  (with-slots (form length) song-sample
    (let ((sample (typecase form
		    (string (read-sample form))
		    (t (ignore-errors (eval form))))))
      (when (sample-p sample)
	(setf length (sample-time sample)))
      sample)))


(defun build-song (filename song)
  "A song is a list of song-sample
- Time is the time where to insert the form.
- Form is :
  - a string: read the associated file and merge it into filename
  - everything else: eval and merge the result if it's a sample.
- Pos is the vertical position for a GUI interface.
- Tags is a symbol or a list of symbol to identify the sample
- Color is the sample color representation
- Length is the sample length in seconds"
  (flet ((display-sample (sample)
	   (with-slots (time form pos tags color length) sample
	     (format t "Time: ~As  pos: ~A  tags: ~S  color: ~6,'0X  length: ~As~%"
		     time pos tags color length)
	     (format t "  ~S~%" form))
	   (force-output)))
    (let ((song (sort (copy-list song) #'(lambda (x y)
					   (< (s-time x)
					      (s-time y)))))
	  (first-sample-p t))
      (when (probe-file filename)
	(delete-file filename))
      (dolist (sample song)
	(display-sample sample)
	(let ((ev-sample (eval-song-sample-form sample)))
	  (when (and (sample-p ev-sample)
		     (>= (s-time sample) 0))
	    (if first-sample-p
		(progn
		  (write-sample filename ev-sample
				:start (s-time sample))
		  (setf first-sample-p nil))
		(mix-sample filename ev-sample
			    #'(lambda (index s1 s2)
				(declare (ignorable index))
				(+ s1 s2))
			    :start (s-time sample)))))))))

(defun build-song-in-interval (filename song begin end)
  (build-song filename 
	      (loop for sample in song
		    when (or (not (sample-p (eval-song-sample-form sample)))
			     (<= begin (s-time sample) end))
		    collect (copy-song-sample sample :time (- (s-time sample) begin)))))
  


(defun list-to-song (list)
  (mapcar #'(lambda (elem)
	      (destructuring-bind (time form pos tags color length) elem
		(make-instance 'song-sample
			       :time (or time 0)
			       :form form
			       :pos (or pos 0)
			       :tags tags
			       :color (or color #x00FF00)
			       :length (or length 0.2))))
	  list))

(defmacro with-build-song ((filename) &rest body)
  `(build-song ,filename (list-to-song ',body)))

(defmacro with-list-song (() &rest body)
  `(list-to-song ',body))


(defun write-song (filename song)
  (with-open-file (stream filename :direction :output
			  :if-exists :supersede
			  :if-does-not-exist :create)
    (format stream ";;;; -*- Mode: Lisp -*-")
    (format stream "~&~%(setf ~S" '*current-song*)
    (format stream "~&      (with-list-song ()~%")
    (dolist (sample song)
      (format stream "~&        (~S ~S ~S ~S #x~6,'0X ~S)"
	      (s-time sample)
	      (s-form sample)
	      (s-pos sample)
	      (s-tags sample)
	      (s-color sample)
	      (s-length sample)))
    (format stream "))~%")))

(defun read-song (filename)
  (with-open-file (stream filename :direction :input)
    (load filename))
  *current-song*)
