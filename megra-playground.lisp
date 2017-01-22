(load "megra-dispatchers")
(load "megra-constructors")

					; define test graph structures
(defparameter *test-graph-1*
  (graph 'uno
       (node 1 (string-event "G1N1E1") (string-event "G1N1E2"))
       (node 2 (string-event "G1N2E1") (string-event "G1N2E2"))
       (edge 1 2 :prob 100 :dur 2000)
       (edge 2 1 :prob 100 :dur 2000)))

(defparameter *test-graph-2*
  (graph 'due
       (node 1 (string-event "G2N1E1") (string-event "G2N1E2"))
       (node 2 (string-event "G2N2E1") (string-event "G2N2E2"))
       (edge 1 2 :prob 100 :dur 2000)
       (edge 2 1 :prob 100 :dur 2000)))

(defparameter *string-dispatcher* (make-instance 'string-dispatcher))

(defparameter *midi-graph*
  (graph 'uno
       (node 1 (midi 65 :lvl .4 :dur 50) (midi 67 :lvl .4 :dur 50))
       (node 2 (midi 61 :lvl .4 :dur 50) (midi 69 :lvl .4 :dur 50))
       (edge 1 2 :prob 100 :dur 2000)
       (edge 2 1 :prob 100 :dur 2000)))

(defparameter *dispatcher* (make-instance 'event-dispatcher))

(incudine:rt-start)

(incudine:now)

(incudine:rt-stop)

(dispatch *dispatcher*
	  *midi-graph*)

(deactivate *midi-graph*)
