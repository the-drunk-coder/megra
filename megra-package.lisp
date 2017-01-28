(require 'cm)
(require 'closer-mop)
;;(require 'incudine)

(defpackage "MEGRA"
  (:use "COMMON-LISP" "CM" "SB-MOP")
  (:export "graph"
	   "brownian-motion"
	   "dispatch"
	   "node"
	   "edge"
	   "mid"
	   "megra-init"
	   "megra-stop"))

(in-package :megra)

(defparameter *processor-directory* (make-hash-table :test 'eql))

(load "megra-events")
(load "megra-structures")
(load "megra-event-processors")
(load "megra-dispatchers")
(load "megra-constructors")
(load "megra-init")
