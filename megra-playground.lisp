(load "megra-dispatchers")
(load "megra-constructors")

					; define test graph structure

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

(setf (successor *test-graph-1*) *test-graph-2*)


(defun graph (name &rest graphdata)
  (let ((new-graph (make-instance 'graph)))
    (setf (graph-id new-graph) name)
    (mapc #'(lambda (obj)
	      (cond ((typep obj 'edge) (insert-edge new-graph obj))
		    ((typep obj 'node) (insert-node new-graph obj))))
	  graphdata)2
    (make-instance 'graph-event-processor :graph new-graph :current-node 1 )))


(defparameter *dispatcher* (make-instance 'string-dispatcher))

(incudine:rt-start)
(incudine:rt-stop)

(dispatch *dispatcher* *test-graph-1* (incudine:now))

(setf (is-active *test-graph-1*) NIL)
