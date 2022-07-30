;;; CL-Wav-Synth -  Manipulate WAV files
;;;
;;; Copyright (C) 2006 Philippe Brochard (hocwp@free.fr)
;;;
;;; #Date#: Wed Jan 31 23:06:42 2007
;;;
;;; **********************************************
;;; The authors grant you the rights to distribute
;;; and use this software as governed by the terms
;;; of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html),
;;; known as the LLGPL.
;;; **********************************************


(in-package :cl-wav-synth)

(defclass next-tutorial-class () ())

(defparameter *hit-delay* 0)



(defparameter *current-def-tuto* 0)
(defparameter *tutorial-state* 0) 
(defparameter *tutorial-hash* (make-hash-table))



(defun write-msg (string clear &optional (skip-sleep #\_))
  (when clear
    (clim-listener::com-clear-output))
  (clim-listener::print-listener-prompt *standard-output* clim:*application-frame*)
  (force-output)
  (loop for c across string
	with do-sleep = t do
	(if (eql c skip-sleep)
	    (setf do-sleep (not do-sleep))
	    (princ c))
	(force-output)
	(when do-sleep
	  (sleep *hit-delay*)))
  (terpri))


(defmacro define-tutorial (form1 clear comment &body form2)
  "Define a tutorial: if form1 is a string, then print form1 (with delay)
and evaluate form2. Else print form1 and evaluate form1 (form2 is ignored)"
  `(progn
    (setf (gethash *current-def-tuto* *tutorial-hash*)
     (list ',form1
      ,comment
      #'(lambda ()
	  ,@(if (stringp form1)
		`((write-msg ,form1 ,clear)
		  (format t "  => ~A~%" ,comment)
		  (force-output)
		  ,@form2)
		`((write-msg (format nil "~(~S~)" ',form1) ,clear)
		  (format t "  => ~A" ,comment)
		  (force-output)
		  (format t " -> ~A~%" ,form1)))
	  (force-output))))
     (incf *current-def-tuto*)))

    

(define-tutorial ",Load As Sample _(pathname)_ synth.wav" t
  "Load a sample in the editor. Notice that you have the auto completion
with Tab or Space key."
  (wav-clim::com-load-as-sample "synth.wav"))

(define-tutorial ",Play" nil
  "Play the current sample"
  (wav-clim::com-play))

(define-tutorial ",Load As Sample _(pathname)_ bomb.wav" t
  "Load a sample in the editor"    
  (wav-clim::com-load-as-sample "bomb.wav"))

(define-tutorial ",Play" nil
  "Play the current sample"
  (wav-clim::com-play))

(define-tutorial ",Set Length _(length)_ 100" t
  "Set the display sample length"
  (wav-clim::com-set-length 100))

(define-tutorial ",Set Index _(index)_ 10000" nil
  "Set the display sample index"
  (wav-clim::com-set-index 10000))

(define-tutorial ",Reset Bound Values" nil
  "Adapt the display to the length of current sample"
  (wav-clim::com-reset-bound-values))

(define-tutorial (help 'set-sample) t
  "Show the help for the function set-sample")

(define-tutorial (set-sample (null-sample 0.01)) nil
  "Set the current sample to a null sample of 0.01 seconds length")

(define-tutorial "_,Reset Bound Values_" nil
  "Adapt the display to the length of current sample"
  (wav-clim::com-reset-bound-values))

(define-tutorial (help 'sinus-sample) t
  "Show the help for the function sinus-sample")

(define-tutorial (set-sample (sinus-sample 0.01 1000)) nil
  "Set the current sample to a sinus sample of 0.01 seconds length
and a frequency of 1000 Hz")

(define-tutorial "_,Reset Bound Values_" nil
  "Adapt the display to the length of current sample"
  (wav-clim::com-reset-bound-values))

(define-tutorial (set-sample (noise-sample 1 100)) t
  "Set the current sample to a noise sample of 0.01 seconds length
and a frequency of 100 Hz")

(define-tutorial "_,Reset Bound Values_" nil
  "Adapt the display to the length of current sample"
  (wav-clim::com-reset-bound-values))

(define-tutorial (set-sample (line-sample* 1 200 :ampls '((0 0)
							  (0.1 10000)
							  (0.2 5000)
							  (0.4 30000)
							  (0.5 -5000)
							  (0.6 5000)
							  (0.7 -10000)
							  (0.8 -5000)
							  (0.9 -20000)
							  (1 0))))
    t
  "Set the current sample to a line sample defined with ampls parameters.
For the first parameter of each control point, 0 is the period begining and
1 is the end. The second parameter is the amplitude value")

(define-tutorial ",Set Length _(length)_ 200" t
    "Set the display sample length"
  (wav-clim::com-set-length 200))

(define-tutorial ",Set Amplitude _(amplitude)_ 40000" nil
    "Set the display sample amplitude"
  (wav-clim::com-set-amplitude 40000))

(define-tutorial "_,Reset Bound Values_" nil
    "Adapt the display to the length of current sample"
  (wav-clim::com-reset-bound-values))

(define-tutorial (set-sample (env it (0 0) (0.1 1))) t
    "Set the current sample envelope. First parameter in second
and the second parameter is the amplitude multiplicator")

(define-tutorial (set-sample (env it (0.1 1) (0.2 0.68))) t
    "Set the current sample envelope. First parameter in second
and the second parameter is the amplitude multiplicator")

(define-tutorial (set-sample (env-sinus it :start 0.2 :period 3 :min 0.5)) t
    "Set the current sample envelope with a sinusoidale envelope.")

(define-tutorial (set-sample (env it (0.8 1) (1 0))) t
    "Set the current sample envelope.")

(define-tutorial ",Play" nil
    "Play the current sample"
  (wav-clim::com-play))

(define-tutorial (set-sample (mix it (env (noise-sample 1.5 200) (0 0) (0.5 0.1) (1.5 1)))) t
    "Mix the current sample and a noise sample of 1.5 seconds length and 200 Hz
with a growing amplitude")

(define-tutorial "_,Reset Bound Values_" nil
    "Adapt the display to the length of current sample"
  (wav-clim::com-reset-bound-values))

(define-tutorial ",Play" nil
    "Play the current sample"
  (wav-clim::com-play))



(define-tutorial ",Set Length _(Length)_ 150" t
    "Set the current length"
  (wav-clim::com-set-length 150))

(define-tutorial ",Set Index _(Index)_ 10000" nil
    "Set the current index"
  (wav-clim::com-set-index 10000))
  
(define-tutorial ",Add Sample Flag _(Number)_ 1 _(Index)_ 10054 _(Amplitude)_ 19660" t
    "Add a first sample flag"
  (wav-clim::com-add-sample-flag 1 10054 19660))


(define-tutorial ",Add Sample Flag _(Number)_ 2 _(Index)_ 10100 _(Amplitude)_ 0" nil
    "Add a second sample flag"
  (wav-clim::com-add-sample-flag 2 10100 0))

(define-tutorial (fi 1) t
    "Get the index for the first sample flag")

(define-tutorial (fi 2) nil
    "Get the index for the second sample flag")

(define-tutorial (setf (fi 2) 10032) t
    "Set the second flag index")

(define-tutorial (fi 2) nil
    "Get the index for the second sample flag")

(define-tutorial (ft 2) nil
    "Get the second flag time in seconds")

(define-tutorial (fa 2) t
    "Get the second flag amplitude")

(define-tutorial (setf (fa 2) 8000) nil
    "Set the second flag amplitude")

(define-tutorial (fa 2) nil
    "Get the second flag amplitude")



(define-tutorial (help 'cut-i) t
    "Show the cut-i function help")

(define-tutorial (set-sample (cut-i it (fi 2) (fi 1))) nil
    "Remove sample from flag 2 index to flag 1 index")

(define-tutorial ",Delete All Sample Flags" t
    "Delete All Sample Flags"
  (wav-clim::com-delete-all-sample-flags))

(define-tutorial "_,Reset Bound Values_" nil
    "Adapt the display to the length of current sample"
  (wav-clim::com-reset-bound-values))


(define-tutorial (help 'octavify) t
    "Show the octavify help")

(define-tutorial (set-sample (octavify it 1)) nil
    "Set the sample to all tones")

(define-tutorial "_,Reset Bound Values_" nil
    "Adapt the display to the length of current sample"
  (wav-clim::com-reset-bound-values))

(define-tutorial ",Play" nil
    "Play the current sample"
  (wav-clim::com-play))

(define-tutorial (undo) t
    "Undo the last effect")

(define-tutorial (set-sample (octave-up it)) t
    "Set the sample to the next octave tone")

(define-tutorial "_,Reset Bound Values_" nil
    "Adapt the display to the length of current sample"
  (wav-clim::com-reset-bound-values))

(define-tutorial ",Play" nil
    "Play the current sample"
  (wav-clim::com-play))

(define-tutorial (undo) t
    "Undo the last effect")

(define-tutorial (set-sample (octavify it 0.8 '(:do :do :do :re :mi :re :do :mi :re :re :do))) t
    "Set the sample to 'Au clair de la lune'")

(define-tutorial "_,Reset Bound Values_" nil
    "Adapt the display to the length of current sample"
  (wav-clim::com-reset-bound-values))

(define-tutorial ",Play" nil
    "Play the current sample"
  (wav-clim::com-play))


(define-tutorial "End of tutorial" t "Thanks to have looked at this tutorial" nil)




(define-wav-function tutorial (&optional (hit-delay 0.1))
  "Run a tutorial to demonstrate a use of the sample editor"
  (setf *hit-delay* hit-delay
	*tutorial-state* 0)
  (next-tutorial))

(define-wav-function next-tutorial ()
  "Run the next tutorial step"
  (let ((fun (gethash *tutorial-state* *tutorial-hash*)))
    (if fun
	(progn
	  (funcall (third fun))
	  (incf *tutorial-state*)
	  (clim:present "Next tutorial" 'next-tutorial-class))
	(setf *tutorial-state* 0))))



(defun auto-doc-tutorial-text (filename)
  "automatically create a tutorial documentation of cl-wav-synth in text form"
  (with-open-file (stream filename :direction :output
			  :if-exists :supersede
			  :if-does-not-exist :create)
    (format stream "CL-WAV-SYNTH: Express noises as you think~%~%~%")
    (format stream "This is a copy of the sample tutorial available with
the 'Tutorial' command in cl-wav-synth.~%~%~%")
    (maphash #'(lambda (k v)
		 (declare (ignore k))
		 (clim-listener::print-listener-prompt stream clim:*application-frame*)
		 (if (stringp (first v))
		     (format stream "~A~%" (first v))
		     (format stream "~(~S~)~%" (first v)))
		 (format stream "  => ~A~%~%" (second v)))
	     *tutorial-hash*)
    (format stream "~%~%(This documentation has been automatically generated with the
function auto-doc-tutorial)~%~%")))



(defun subst-in-tree-from-doc (original-tree)
  "Add reference in the cl-wav-synth documentation"
  (let ((string (format nil "~(~S~)" original-tree))
	(found '()))
    (labels ((rec (tree)
	       (if (atom tree)
		   (multiple-value-bind (val foundp)
		       (gethash tree *wav-function-hash*)
		     (declare (ignore val))
		     (when (and foundp (not (member tree found)))
		       (setf string
			     (subst-string (format nil "<a href=\"documentation.html#~A\">~(~S~)</a>"
						   (escape-name (format nil "~S" tree))
						   tree)
					   (format nil "~(~S~)" tree)
					   string))
		       (push tree found)))
		   (progn
		     (rec (car tree))
		     (when (cdr tree)
		       (rec (cdr tree)))))))
      (rec original-tree)
      string)))



(defun auto-doc-tutorial-html (filename)
    "automatically create a tutorial documentation of cl-wav-synth in html form"
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
    (format stream "<h2>This is a copy of the sample tutorial available with
the 'Tutorial' command in cl-wav-synth.</h2>")
    (maphash #'(lambda (k v)
		 (declare (ignore k))
		 (format stream "<p>")
		 (clim-listener::print-listener-prompt stream clim:*application-frame*)
		 (if (stringp (first v))
		     (format stream "~A~%" (first v))
		     (format stream (subst-in-tree-from-doc (first v))))
		 (format stream "<br/>&nbsp;&nbsp;&nbsp;&nbsp;")
		 (format stream "  => ~A~%~%" (second v))
		 (format stream "</p>"))
	     *tutorial-hash*)
    (format stream "<p><small>(This documentation has been automatically generated with the
function auto-doc-tutorial)</small></p>~%~%")
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


(define-wav-function auto-doc-tutorial (filename &optional (type :text))
  "automatically create a tutorial documentation of cl-wav-synth. Type can be :text or :html"
  (format t "~&Generating: ~A~%" filename)
  (ecase type
    (:text (auto-doc-tutorial-text filename))
    (:html (auto-doc-tutorial-html filename))))




(define-wav-function auto-doc-all (&optional (directory "Doc/"))
  "Automatically generate all cl-wav-synth documentations: main documentation
and tutorial documentation"
  (labels ((make-filename (name type)
	     (make-pathname :host (pathname-host directory)
			    :device (pathname-device directory)
			    :directory (pathname-directory directory)
			    :name name
			    :type type)))
    (ensure-directories-exist directory)
    (auto-doc (make-filename "documentation" "txt") :text)
    (auto-doc (make-filename "documentation" "html") :html)
    (auto-doc-tutorial (make-filename "tutorial" "txt") :text)
    (auto-doc-tutorial (make-filename "tutorial" "html") :html)))
  




(in-package :cl-wav-synth-clim)

(add-menu-item-to-command-table 'help-command-table "Tutorial" :divider nil)

(define-command (com-tutorial :name t :menu "Run the tutorial"
			      :command-table help-command-table)
    ()
  (cl-wav-synth::tutorial))

(define-command (com-next-tutorial :name t :menu "Next tutorial"
				   :command-table help-command-table)
    ()
  (cl-wav-synth::next-tutorial))

(define-presentation-to-command-translator next-tutorial-transl
    (cl-wav-synth::next-tutorial-class com-next-tutorial help-command-table
				       :gesture :select
				       :documentation "Next tutorial"
				       :pointer-documentation "Next tutorial")
    ()
  ())


(add-menu-item-to-command-table 'help-command-table "Documentation" :divider nil)

(define-command (com-auto-doc-all :name t :menu "Produce all documentation"
				  :command-table help-command-table)
    ()
  (format t "Auto producing all documentation in ./Doc/")
  (cl-wav-synth::auto-doc-all)
  (force-output))


(add-command-table-to-listener 'help-command-table)
