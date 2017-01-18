(load "megra-dispatchers")
					; define test graph structure
(defun populate-testgraph ()
					; setup
  (defparameter *test-graph-1* (make-instance 'graph))
  (defparameter *test-graph-processor-1* (make-instance 'graph-event-processor))

  (setf (graph-id *test-graph-1*) 'test-graph-1)
  (setf (source-graph *test-graph-processor-1*) *test-graph-1*)

  (defparameter *test-graph-2* (make-instance 'graph))
  (defparameter *test-graph-processor-2* (make-instance 'graph-event-processor))

  (setf (graph-id *test-graph-2*) 'test-graph-2)
  (setf (source-graph *test-graph-processor-2*) *test-graph-2*)

					;graph 1
  (insert-node *test-graph-1* (make-instance 'node :id 1 :content `(,(make-instance 'string-event :msg "G1N1"))))
  (insert-node *test-graph-1* (make-instance 'node :id 2 :content `(,(make-instance 'string-event :msg "G1N2"))))
  (insert-node *test-graph-1* (make-instance 'node :id 3 :content `(,(make-instance 'string-event :msg "G1N3"))))
  (insert-node *test-graph-1* (make-instance 'node :id 4 :content `(,(make-instance 'string-event :msg "G1N4"))))
  (insert-node *test-graph-1* (make-instance 'node :id 5 :content `(,(make-instance 'string-event :msg "G1N5"))))
  (insert-edge *test-graph-1* (make-instance 'edge :src 1 :dest 1 :prob 40 :content `(,(make-instance 'transition :dur 200))))
  (insert-edge *test-graph-1* (make-instance 'edge :src 1 :dest 2 :prob 15 :content `(,(make-instance 'transition :dur 200))))
  (insert-edge *test-graph-1* (make-instance 'edge :src 1 :dest 3 :prob 15 :content `(,(make-instance 'transition :dur 200))))
  (insert-edge *test-graph-1* (make-instance 'edge :src 1 :dest 4 :prob 15 :content `(,(make-instance 'transition :dur 200))))
  (insert-edge *test-graph-1* (make-instance 'edge :src 1 :dest 5 :prob 15 :content `(,(make-instance 'transition :dur 200))))
  (insert-edge *test-graph-1* (make-instance 'edge :src 2 :dest 1 :prob 100 :content `(,(make-instance 'transition :dur 200))))
  (insert-edge *test-graph-1* (make-instance 'edge :src 3 :dest 1 :prob 100 :content `(,(make-instance 'transition :dur 200))))
  (insert-edge *test-graph-1* (make-instance 'edge :src 4 :dest 1 :prob 100 :content `(,(make-instance 'transition :dur 200))))
  (insert-edge *test-graph-1* (make-instance 'edge :src 5 :dest 1 :prob 100 :content `(,(make-instance 'transition :dur 200))))
					; graph 2
  (insert-node *test-graph-2* (make-instance 'node :id 1 :content `(,(make-instance 'string-event :msg "G2N1"))))
  (insert-node *test-graph-2* (make-instance 'node :id 2 :content `(,(make-instance 'string-event :msg "G2N2"))))
  (insert-node *test-graph-2* (make-instance 'node :id 3 :content `(,(make-instance 'string-event :msg "G2N3"))))
  (insert-node *test-graph-2* (make-instance 'node :id 4 :content `(,(make-instance 'string-event :msg "G2N4"))))
  (insert-node *test-graph-2* (make-instance 'node :id 5 :content `(,(make-instance 'string-event :msg "G2N5"))))
  (insert-edge *test-graph-2* (make-instance 'edge :src 1 :dest 1 :prob 40 :content `(,(make-instance 'transition :dur 1000))))
  (insert-edge *test-graph-2* (make-instance 'edge :src 1 :dest 2 :prob 15 :content `(,(make-instance 'transition :dur 1000))))
  (insert-edge *test-graph-2* (make-instance 'edge :src 1 :dest 3 :prob 15 :content `(,(make-instance 'transition :dur 1000))))
  (insert-edge *test-graph-2* (make-instance 'edge :src 1 :dest 4 :prob 15 :content `(,(make-instance 'transition :dur 1000))))
  (insert-edge *test-graph-2* (make-instance 'edge :src 1 :dest 5 :prob 15 :content `(,(make-instance 'transition :dur 1000))))
  (insert-edge *test-graph-2* (make-instance 'edge :src 2 :dest 1 :prob 100 :content `(,(make-instance 'transition :dur 1000))))
  (insert-edge *test-graph-2* (make-instance 'edge :src 3 :dest 1 :prob 100 :content `(,(make-instance 'transition :dur 1000))))
  (insert-edge *test-graph-2* (make-instance 'edge :src 4 :dest 1 :prob 100 :content `(,(make-instance 'transition :dur 1000))))
  (insert-edge *test-graph-2* (make-instance 'edge :src 5 :dest 1 :prob 100 :content `(,(make-instance 'transition :dur 1000))))
					; set start nodes
  (setf (current-node *test-graph-processor-1*) 1)
  (setf (current-node *test-graph-processor-2*) 1)
  (setf (successor *test-graph-processor-1*) *test-graph-processor-2*)
  )

(populate-testgraph)

(defparameter *dispatcher* (make-instance 'string-dispatcher))

(incudine:rt-start)
(incudine:rt-stop)

(incudine:now)

(dispatch *dispatcher* *test-graph-processor-1*)

