;; the basic structure of music ...
(defclass node ()
  ((global-id :accessor node-global-id)
   (id :accessor node-id :initarg :id)
   (content :accessor node-content :initarg :content)
   (color :accessor node-color :initarg :color :initform 'white)))

(defmethod add-event ((n node) (e event) &key)
  (setf (event-source e) (node-global-id n))  
  (cons e (node-content n)))

;; turn back into textual representation ...
(defmethod print-node ((n node) &key)
  (if (eql (node-color n) 'white)
      (format nil "(node ~a ~a)"
	      (node-id n)
	      (format nil "~{~a ~}" (mapcar #'print-event (node-content n))))
      (format nil "(node-col ~a (:col '~a) ~a)"
	      (node-id n)
	      (node-color n)
	      (format nil "~{~a ~}" (mapcar #'print-event (node-content n))))))

;; only probability is a structural property on this level.
;; duration is better placed on the event level
;; (see migra-events)
;; and placed in the edge content
(defclass edge ()
  ((source :accessor edge-source :initarg :src)
   (destination :accessor edge-destination :initarg :dest)
   (probability :accessor edge-probablity :initarg :prob)
   (content :accessor edge-content :initarg :content)))

;; i could split transition print here, but i think it's ok like that for now ...
;; turn back to textual representation ...
(defmethod print-edge ((e edge) &key)
  (format nil "(edge ~d ~d :prob ~d :dur ~d)"
	  (edge-source e)
	  (edge-destination e)
	  (edge-probablity e)
	  (transition-duration (car (edge-content e)))))

;; compare edges to remove duplicates
(defmethod edge-equals ((a edge) (b edge) &key)
  (and (eql (edge-source a) (edge-source b))
       (eql (edge-destination a) (edge-destination b))))

;; everything can be organized as a graph, really ...
(defclass graph ()
  ((id :accessor graph-id)
   (nodes :accessor graph-nodes)
   (edges :accessor graph-edges)))

(defmethod initialize-instance :after ((g graph) &key)
  (setf (graph-nodes g) (make-hash-table :test 'eql))
  (setf (graph-edges g) (make-hash-table :test 'eql)))

(defmethod insert-node ((g graph) (n node) &key)
  (setf (node-global-id n) (cons (graph-id g) (node-id n)))
  ;; set event source ids with format:
  ;; ((GRAPH-ID . NODE-ID) . EVENT-POS) 
  (if (node-content n)
      (labels ((identify (nodes count)
		 (if nodes
		     (progn
		       (setf (event-source (car nodes)) (cons (node-global-id n) count))
		       (setf (event-tags (car nodes)) (cons (node-color n) (event-tags (car nodes))))
		       (identify (cdr nodes) (+ 1 count))))))
	(identify (node-content n) 0)))
  (setf (gethash (node-id n) (graph-nodes g)) n))

(defmethod insert-edge ((g graph) (e edge) &key)
  (let ((edges (gethash (edge-source e) (graph-edges g))))
    ;; set transition source ids with format:
    ;; ((GRAPH-ID . E) . (SOURCE . DEST)) 
    (if (edge-content e)
	(setf (event-source (car (edge-content e))) 
	      (cons (cons (graph-id g) 'E) (cons (edge-source e) (edge-destination e)))))
    (setf (gethash (edge-source e) (graph-edges g))
	  (remove-duplicates (cons e edges) :test #'edge-equals))))

(defmethod graph-size ((g graph))
  (hash-table-count (graph-nodes g)))

;;(in-package :megra)
(defmethod get-edge ((g graph) source destination)
  (labels ((find-destination (edges destination)
	     ;; might be more efficient to use edge list ordered by
	     ;; destination and use binary search ... 
	     (if (eql (edge-destination (car edges)) destination)
		 (car edges)
		 (find-destination (cdr edges) destination))))
    (let* ((current-edges (gethash source (graph-edges g)  )  )
	  (found-edge (find-destination current-edges destination)))
      found-edge)))

;; regarding probability modification:
;; an event is pumped through the chain and each
;; processor knows the source of the event, as it is stored
;; in the event.
;; so we can modify the edge that led to that event,
;; if applicble 




