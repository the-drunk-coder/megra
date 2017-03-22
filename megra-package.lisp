;(require 'cm)
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
(defparameter *buffer-directory* (make-hash-table :test 'equal))
;; sample root folder 
(defparameter *sample-root* "/home/nik/SAMPLES/" )
;; consecutive buffer numbers for scsynth
(defparameter *sc-sample-bufnums* 0)

;; main storage for event processors
(defparameter *processor-directory* (make-hash-table :test 'eql))

;; the default backend for DSP
;; 'inc -> incudine
;; 'sc -> SuperCollider
(defparameter *default-dsp-backend* 'inc)

(defparameter *pi* 3.14159265359)
;;(defparameter *half-pi* 3.14159265359)

(defparameter *midi-responders* (make-hash-table :test 'eql))

;; load the megra stuff except for dsp ...
(load "megra-events")
(load "megra-structures")
(load "megra-event-processors")
(load "megra-dispatchers")
(load "megra-constructors")
(load "megra-event-filters")
(load "megra-sc-backend")
