(require 'closer-mop)

(defpackage "MEGRA"
 (:use "COMMON-LISP" "CM" "SB-MOP")
 (:export "graph"
	  "brownian-motion"
	  "dispatch"
	  "node"
	  "edge"
	  "mid"
	  "grain"
	  "megra-init"
	  "megra-stop"
	  "handle-event"))

(in-package :megra)

;; helper structure to store sample data
(defstruct buffer-data 
  buffer
  buffer-rate
  buffer-frames)

;; storage for sample buffers 
(defparameter *incu-buffer-directory* (make-hash-table :test 'equal))
(defparameter *sc-buffer-directory* (make-hash-table :test 'equal))
;; sample root folder 
(defparameter *sample-root* "/home/nik/SAMPLES/" )

;; consecutive buffer numbers for scsynth
;; start with two as 0 and 1 are reserved for the reverb buffers ...
(defparameter *sc-sample-bufnums* 2)

(defparameter *eval-on-copy* nil)

;; make int a little bit longer, because the first one or two elements will be dropped
(defparameter *global-trace-length* 9)

(defvar *encourage-percentage* 5)
;; what might be the justification for this split ?
;; "Un-Learning" things is harder for humans, but for machines ?
;;(in-package :megra)
(defvar *discourage-percentage* 5)

(in-package :megra)

;; main storage for event processors (a processor can be used without
;; being kept here, but at least historically it has been practical
;; to keep track of certain things)
(defparameter *processor-directory* (make-hash-table :test 'eql))

;; when branching, keep the previous states ... 
(defparameter *prev-processor-directory* (make-hash-table :test 'eql))

;; chains and branches 
(defparameter *chain-directory* (make-hash-table :test 'eql))
(defparameter *branch-directory* (make-hash-table :test 'eql))

;; chain groups ... 
(defparameter *group-directory* (make-hash-table :test 'eql))


(defparameter *pi* 3.14159265359)

(defparameter *midi-responders* (make-hash-table :test 'eql))
(defparameter *pad-toggle-states* (make-hash-table :test 'eql))

(defparameter *global-azimuth-offset* 0.0)
(defparameter *global-elevation-offset* 0.0)

(defparameter *global-midi-delay* 0.16)

(defparameter *global-osc-delay* 0.12)

(defparameter *current-group* 'DEFAULT)

;; the default backend for DSP
;; 'inc -> incudine
;; 'sc -> SuperCollider

;;(defparameter *default-dsp-backend* 'inc)
(defparameter *default-dsp-backend* 'sc)

;; load the megra stuff except for dsp ...
(load "megra-object-handling")
(load "megra-param-modificators")
(load "megra-event-base")
(load "megra-event-definitions")
(load "megra-supercollider-event-handlers")
(load "megra-incudine-event-handlers")
(load "megra-structures")
(load "megra-event-processors")
(load "megra-disencourage")
(load "megra-dispatchers")
(load "megra-constructors")
(load "megra-event-filters")
(load "megra-supercollider-interface")
(load "megra-visualize")
