;;; CL-Wav-Synth -  Manipulate WAV files
;;;
;;; Copyright (C) 2006 Philippe Brochard (hocwp@free.fr)
;;;
;;; #Date#: Sun Aug 13 11:56:07 2006
;;;
;;; **********************************************
;;; The authors grant you the rights to distribute
;;; and use this software as governed by the terms
;;; of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html),
;;; known as the LLGPL.
;;; **********************************************

(in-package :cl-wav-synth-clim)

(define-command-table build-song-undo-redo-command-table)

(defmacro define-com-song-undo (com-fun menu wav-fun)
  `(define-command (,com-fun :name t :menu ,menu
		    :command-table build-song-undo-redo-command-table)
    ()
    ,wav-fun))

(define-com-song-undo com-song-undo "Undo" (wav::s-undo))
(define-com-song-undo com-song-redo "Redo" (wav::s-redo))
(define-com-song-undo com-song-list-undo "List undo" (wav::s-list-undo))
(define-com-song-undo com-song-save-undo "Save undo" (wav::s-save-undo))
(define-com-song-undo com-song-clear-undo "Clear all undo" (wav::s-clear-undo))

(add-command-table-to-listener 'build-song-undo-redo-command-table)



(define-command-table sample-undo-redo-command-table)

(defmacro define-com-sample-undo (com-fun menu wav-fun)
  `(define-command (,com-fun :name t :menu ,menu
		    :command-table sample-undo-redo-command-table)
    ()
    ,wav-fun))

(define-com-sample-undo com-sample-undo "Undo" (wav::undo))
(define-com-sample-undo com-sample-redo "Redo" (wav::redo))
(define-com-sample-undo com-sample-list-undo "List undo" (wav::list-undo))
(define-com-sample-undo com-sample-save-undo "Save undo" (wav::save-undo))
(define-com-sample-undo com-sample-clear-undo "Clear all undo" (wav::clear-undo))

(add-command-table-to-listener 'sample-undo-redo-command-table)

