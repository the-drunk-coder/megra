(load "megra-events")
					;the basic structure of music
(defclass node ()
  ((global-id :accessor node-global-id)
   (id :accessor node-id :initarg :id)
   (content :accessor node-content :initarg :content)))

(defmethod add-event ((n node) (e event) &key)
  (setf (event-source e) (node-global-id n))
  (cons e (node-content n)))

					; only probability is a structural property on this level.
					; duration is better placed on the event level
					; (see migra-events)
					; and placed in the edge content
(defclass edge ()
  ((source :accessor edge-source :initarg :src)
   (destination :accessor edge-destination :initarg :dest)
   (probability :accessor edge-probablity :initarg :prob)
   (content :accessor edge-content :initarg :content)))

					; compare edges to remove duplicates
(defmethod edge-equals ((a edge) (b edge) &key)
  (and (eql (edge-source a) (edge-source b))
       (eql (edge-destination a) (edge-destination b))))

					; everything can be organized as a graph
(defclass graph ()
  ((id :accessor graph-id)
   (nodes :accessor graph-nodes)
   (edges :accessor graph-edges)))

(defmethod initialize-instance :after ((g graph) &key)
  (setf (graph-nodes g) (make-hash-table :test 'eql))
  (setf (graph-edges g) (make-hash-table :test 'eql)))

; tbd: exact source addressing (graph-node-position)
(defmethod insert-node ((g graph) (n node) &key)
  (setf (node-global-id n) (cons (graph-id g) (node-id n)))
  (if (node-content n)
      (mapc #'(lambda (e)
	      (setf (event-source e) (node-global-id n)))
	    (node-content n))); set event source ids 
  (setf (gethash (node-id n) (graph-nodes g)) n))

(defmethod insert-edge ((g graph) (e edge) &key)
  (let ((edges (gethash (edge-source e) (graph-edges g))))
    (setf (gethash (edge-source e) (graph-edges g))
	  (remove-duplicates (cons e edges) :test #'edge-equals))))

(defmethod graph-size ((g graph))
  (hash-table-count (graph-nodes g)))



					; regarding probability modification:
					; an event is pumped through the chain and each
					; processor knows the source of the event, as it is stored
					; in the event.
					; so we can modify the edge that led to that event,
					; if applicble 




