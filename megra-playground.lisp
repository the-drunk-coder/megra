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


(defparameter *dispatcher* (make-instance 'string-dispatcher))

(incudine:rt-start)

(incudine:now)

(incudine:rt-stop)

(dispatch *dispatcher*
	  *test-graph-1*
	  *test-graph-2*)


(deactivate *test-graph-1*)
