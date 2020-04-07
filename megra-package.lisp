;; load dependencies
(ql:quickload :closer-mop)
(ql:quickload :cl-fad)
(ql:quickload :cl-libsndfile)
(ql:quickload :vom)
(ql:quickload :cl-ppcre)

(require 'closer-mop)
(require 'cl-fad)
(require 'cl-libsndfile)
(require 'vom)
(require 'cl-ppcre)

(defpackage "MEGRA"
 (:use "COMMON-LISP" "CM" "SB-MOP" "CL-FAD" "CL-LIBSNDFILE")
 (:export "graph"
	  "brownian-motion"
	  "dispatch"
	  "node"
	  "edge"
	  "mid"
	  "grain"
	  "megra-init"
	  "megra-stop"
	  "handle-event"
	  "EVENTS"))

(in-package :megra)

;; helper structure to store sample data
(defstruct buffer-data 
  buffer
  buffer-rate
  buffer-frames)

;; storage for sample buffers 
(defparameter *sc-buffer-directory* (make-hash-table :test 'equal))

;; consecutive buffer numbers for scsynth
;; start with two as 0 and 1 are reserved for the reverb buffers ...
(defparameter *sc-sample-bufnums* 2)

(defparameter *eval-on-copy* nil)

;; make int a little bit longer, because the first one or two elements will be dropped
(defparameter *global-trace-length* 10)

(defvar *global-tempo-mod* 1.0)

(defun tmod (mod) (setf *global-tempo-mod* mod))

(defvar *encourage-percentage* 5)

;; what might be the justification for this split ?
;; "Un-Learning" things is harder for humans, but for machines ?
(defvar *discourage-percentage* 5)

(in-package :megra)

;; main storage for stateful event processors (a processor can be used without
;; being kept here, but at least historically it has been practical
;; to keep track of certain things)
(defparameter *processor-directory* (make-hash-table :test 'eql))

(defparameter *global-silence-symbol* '~)

;; chains 
(defparameter *global-syncs* (make-hash-table :test 'eql))
(defparameter *multichain-directory* (make-hash-table :test 'eql))

(defparameter *pi* 3.14159265359)

(defparameter *midi-responders* (make-hash-table :test 'eql))
(defparameter *pad-toggle-states* (make-hash-table :test 'eql))

(defparameter *global-azimuth-offset* 0.0)
(defparameter *global-elevation-offset* 0.0)

(defparameter *global-midi-delay* 0.16)

(defparameter *global-osc-delay* 0.24)

(defparameter *global-default-duration* 200)

(defun global-dur (new-duration)
  "Set a global default duration. Already constructed entities won't be affected."
  (setf *global-default-duration* new-duration))

(defun global-bpm (bpm)
  "Set a global tempo (approximate). Already constructed entities won't be affected."
  (setf *global-default-duration* (coerce 'integer (/ 60000 bpm))))

;; the default backend for DSP
;; 'inc -> incudine
;; 'sc -> SuperCollider

;; redefine sample root in megra package ...
(defparameter *sample-root* cm::*sample-root*)

;; only option for now ...
(defparameter *default-dsp-backend* 'sc)

;; load the megra stuff except for dsp ...
(load (compile-file (concatenate 'string cm::*megra-root* "/megra-object-handling")))
(load (compile-file (concatenate 'string cm::*megra-root* "/megra-param-modificators")))
(load (concatenate 'string cm::*megra-root* "/megra-event-base"))
(load (concatenate 'string cm::*megra-root* "/megra-event-definitions"))
(load (compile-file (concatenate 'string cm::*megra-root* "/megra-pitch-arithmetic")))
(load (compile-file (concatenate 'string cm::*megra-root* "/megra-supercollider-event-handlers-2ch")))
(load (compile-file (concatenate 'string cm::*megra-root* "/megra-supercollider-event-handlers-4ch")))
(load (compile-file (concatenate 'string cm::*megra-root* "/megra-supercollider-event-handlers-8ch")))
(load (compile-file (concatenate 'string cm::*megra-root* "/megra-supercollider-event-handlers-16ch")))
(load (compile-file (concatenate 'string cm::*megra-root* "/megra-supercollider-event-handlers-24ch")))
(load (compile-file (concatenate 'string cm::*megra-root* "/megra-supercollider-event-handlers-32ch")))
(load (compile-file (concatenate 'string cm::*megra-root* "/megra-supercollider-event-handlers-ambi")))
(load (compile-file (concatenate 'string cm::*megra-root* "/megra-helpers")))
(load (compile-file (concatenate 'string cm::*megra-root* "/megra-event-processor-base")))
(load (compile-file (concatenate 'string cm::*megra-root* "/megra-generator")))
(load (compile-file (concatenate 'string cm::*megra-root* "/megra-generator-growth")))
(load (compile-file (concatenate 'string cm::*megra-root* "/megra-generator-generators")))
(load (compile-file (concatenate 'string cm::*megra-root* "/megra-event-processor-wrappers")))
(load (compile-file (concatenate 'string cm::*megra-root* "/megra-lifemodel")))
(load (compile-file (concatenate 'string cm::*megra-root* "/megra-probctrl")))
(load (compile-file (concatenate 'string cm::*megra-root* "/megra-direct-modifiers")))
;;(load (compile-file (concatenate 'string cm::*megra-root* "/megra-stream-event-processors")))
;;(load (compile-file (concatenate 'string cm::*megra-root* "/megra-disencourage")))
(load (compile-file (concatenate 'string cm::*megra-root* "/megra-dispatchers")))
(load (compile-file (concatenate 'string cm::*megra-root* "/megra-deepcopy")))
;;(load (compile-file (concatenate 'string cm::*megra-root* "/megra-constructors")))
(load (compile-file (concatenate 'string cm::*megra-root* "/megra-event-filters")))
(load (compile-file (concatenate 'string cm::*megra-root* "/megra-supercollider-interface")))
(load (concatenate 'string cm::*megra-root* "/megra-generate-sample-category-events"))
