;;; CL-Wav-Synth -  Manipulate WAV files
;;;
;;; Copyright (C) 2006 Philippe Brochard (hocwp@free.fr)
;;;
;;; #Date#: Fri Jun  2 23:30:56 2006
;;;
;;; **********************************************
;;; The authors grant you the rights to distribute
;;; and use this software as governed by the terms
;;; of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html),
;;; known as the LLGPL.
;;; **********************************************

(in-package :cl-user)

(defpackage :uni-shell
  (:use :common-lisp)
  (:export :dbg
	   :urun-prog
	   :ushell
	   :ush
	   :ushell-loop
	   :uquit
	   :set-shell-dispatch)
  (:nicknames :ushell))

(in-package :uni-shell)


(push :uni-shell *features*)


(defvar *%dbg-name%* "dbg")
(defvar *%dbg-count%* 0)


(defmacro dbg (&rest forms)
  `(progn
    ,@(mapcar #'(lambda (form)
		  (typecase form
		    (string `(setf *%dbg-name%* ,form))
		    (number `(setf *%dbg-count%* ,form))))
	      forms)
    (format t "~&DEBUG[~A - ~A]  " (incf *%dbg-count%*) *%dbg-name%*)
    ,@(mapcar #'(lambda (form)
		  (typecase form
		    ((or string number) nil)
		    (t `(format t "~A=~S   " ',form ,form))))
	      forms)
    (format t "~%")
    (force-output)))

(defmacro dbgt (&rest forms)
  `(progn
    ,@(mapcar #'(lambda (form)
		  (typecase form
		    (string `(setf *%dbg-name%* ,form))
		    (number `(setf *%dbg-count%* ,form))))
	      forms)
    (format *terminal-io* "~&DEBUG[~A - ~A]  " (incf *%dbg-count%*) *%dbg-name%*)
    ,@(mapcar #'(lambda (form)
		  (typecase form
		    ((or string number) nil)
		    (t `(format *terminal-io* "~A=~S   " ',form ,form))))
	      forms)
    (format *terminal-io* "~%")
    (force-output)))




(defun uquit ()
  #+(or clisp cmu) (ext:quit)
  #+sbcl (sb-ext:quit)
  #+ecl (si:quit)
  #+gcl (lisp:quit)
  #+lispworks (lw:quit)
  #+(or allegro-cl allegro-cl-trial) (excl:exit))
  
  


(defun remove-plist (plist &rest keys)
  "Remove the keys from the plist.
Useful for re-using the &REST arg after removing some options."
  (do (copy rest)
      ((null (setq rest (nth-value 2 (get-properties plist keys))))
       (nreconc copy plist))
    (do () ((eq plist rest))
      (push (pop plist) copy)
      (push (pop plist) copy))
    (setq plist (cddr plist))))


(defun split-string (string &optional (separator #\Space))
  (loop for i = 0 then (1+ j)
        as j = (position separator string :start i)
        as sub = (subseq string i j)
        unless (string= sub "") collect sub
        while j))



(defun urun-prog (prog &rest opts &key args (wait t) &allow-other-keys)
  "Common interface to shell. Does not return anything useful."
  #+gcl (declare (ignore wait))
  (setq opts (remove-plist opts :args :wait))
  #+allegro (apply #'excl:run-shell-command (apply #'vector prog prog args)
                   :wait wait opts)
  #+(and clisp      lisp=cl)
  (apply #'ext:run-program prog :arguments args :wait wait opts)
  #+(and clisp (not lisp=cl))
  (if wait
      (apply #'lisp:run-program prog :arguments args opts)
      (lisp:shell (format nil "~a~{ '~a'~} &" prog args)))
  #+cmu (apply #'ext:run-program prog args :wait wait :output *standard-output* opts)
  #+gcl (apply #'si:run-process prog args)
  #+liquid (apply #'lcl:run-program prog args)
  #+lispworks (apply #'sys::call-system-showing-output
                     (format nil "~a~{ '~a'~}~@[ &~]" prog args (not wait))
                     opts)
  #+lucid (apply #'lcl:run-program prog :wait wait :arguments args opts)
  #+sbcl (apply #'sb-ext:run-program prog args :wait wait :output *standard-output* opts)
  #-(or allegro clisp cmu gcl liquid lispworks lucid sbcl)
  (error 'not-implemented :proc (list 'run-prog prog opts)))


;;(defparameter *shell-cmd* "/usr/bin/env")
;;(defparameter *shell-cmd-opt* nil)

#+UNIX (defparameter *shell-cmd* "/bin/sh")
#+UNIX (defparameter *shell-cmd-opt* '("-c"))

#+WIN32 (defparameter *shell-cmd* "cmd.exe")
#+WIN32 (defparameter *shell-cmd-opt* '("/C"))


(defun ushell (&rest strings)
  (urun-prog *shell-cmd* :args (append *shell-cmd-opt* strings)))

(defun ush (string)
  (urun-prog *shell-cmd* :args (append *shell-cmd-opt* (list string))))


(defun set-shell-dispatch (&optional (shell-fun 'ushell))
  (labels ((|shell-reader| (stream subchar arg)
	     (declare (ignore subchar arg))
	     (list shell-fun (read stream t nil t))))
    (set-dispatch-macro-character #\# #\# #'|shell-reader|)))


(defun ushell-loop (&optional (shell-fun #'ushell))
  (loop
   (format t "UNI-SHELL> ")
   (let* ((line (read-line)))
     (cond ((zerop (or (search "quit" line) -1)) (return))
	   ((zerop (or (position #\! line) -1))
	    (funcall shell-fun (subseq line 1)))
	   (t (format t "~{~A~^ ;~%~}~%"
		      (multiple-value-list 
		       (ignore-errors (eval (read-from-string line))))))))))
