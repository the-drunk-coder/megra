					; generic event-generator			        
(defclass event-processor ()
  ((pull-events)
   (pull-transition)       
   (active :accessor is-active :initform nil)
   (successor :accessor successor)
   (has-successor)
   (current-events)      ; abstract
   (current-transition)  ; abstract   
   ))

(defmethod has-successor ((e event-processor) &key)
  (slot-boundp e 'successor))

(defmethod pull-events ((e event-processor) &key)
  (if (has-successor e)
      (apply-self e (pull-events (successor e)))
      (current-events e)))

(defmethod pull-transition ((e event-processor) &key)
  (if (has-successor e)
      (progn
	(current-transition e)
	(pull-transition (successor e)))
      (current-transition e)))

					; dummy for testing, development and debugging ..
(defclass dummy-event-processor (event-processor)
  ((name :accessor dummy-name)))

(defmethod apply-self ((e dummy-event-processor) events &key)
  (fresh-line)
  (princ "applying ")
  (current-events e))

(defmethod current-events ((e dummy-event-processor) &key)
  (fresh-line)
  (princ "dummy events from ")
  (princ (dummy-name e)))

(defmethod current-transition ((e dummy-event-processor) &key)
  (fresh-line)
  (princ "dummy transition from ")
  (princ (dummy-name e)))

					; graph-based event-generator ... 
(defclass graph-event-processor (event-processor) 
  ((source-graph :accessor source-graph :initarg :graph)
   (current-node :accessor current-node :initarg :current-node)
   (path)))

;; strange mop-method to allow cloning events
(defmethod copy-instance (object)
   (let ((copy (allocate-instance (class-of object))))
     (loop for slot in (class-slots (class-of object))
	do (when (slot-boundp-using-class (class-of object) object slot)
	     (setf (slot-value copy (slot-definition-name slot))
		   (slot-value object (slot-definition-name slot)))))
     copy))

(defmethod current-events ((g graph-event-processor) &key)
  (mapcar #'copy-instance (node-content (gethash (current-node g) (graph-nodes (source-graph g))))))

					; get the transition and set next current node
(defmethod current-transition ((g graph-event-processor) &key)
  (labels
      ((choice-list (edge counter)
	 (loop repeat (edge-probablity edge)
	    collect counter))
       (collect-choices (edges counter)
	 (if edges
	     (append (choice-list (car edges) counter) (collect-choices (cdr edges) (1+ counter)))
	     '())))
  (let* ((current-edges (gethash (current-node g) (graph-edges (source-graph g))))
	 (current-choices (collect-choices current-edges 0))
	 (chosen-edge-id (nth (random (length current-choices)) current-choices))
	 (chosen-edge (nth chosen-edge-id current-edges)))
    (setf (current-node g) (edge-destination chosen-edge))
    (car (edge-content chosen-edge)))))

(defmethod apply-self ((g graph-event-processor) events &key)
  (combine-events (current-events g) events))

(defclass modifying-event-processor (event-processor)
  ((property :accessor modified-property :initarg :mod-prop)
   (last-values-by-source :accessor lastval)))

;; pass
(defmethod current-transition ((m modifying-event-processor) &key))

(defmethod initialize-instance :after ((m modifying-event-processor) &key)
  (setf (lastval m) (make-hash-table :test 'eql)))

(defmethod apply-self :before ((m modifying-event-processor) events &key)
  (mapc #'(lambda (event)
	    (unless (gethash (event-source event) (lastval m))  
	      (setf (gethash (event-source event) (lastval m))
		    (funcall (symbol-function (modified-property m)) event)))) events))


					; switch to preserve/not preserve state ?
(defclass oscillate-between (modifying-event-processor)
  ((upper-boundary :accessor upper-boundary :initarg :upper-boundary)
   (lower-boundary :accessor lower-boundary :initarg :lower-boundary)   
   (type :accessor osc-type :initarg :type)))

(defclass brownian-motion (modifying-event-processor)
  ((upper-boundary :accessor upper-boundary :initarg :upper-boundary)
   (lower-boundary :accessor lower-boundary :initarg :lower-boundary)
   (step-size :accessor step-size :initarg :step)
   (is-bounded :accessor is-bounded :initarg :is-bounded)
   (is-wrapped :accessor is-wrapped :initarg :is-wrapped)))

(defmethod cap ((b brownian-motion) value &key)
  (cond ((is-bounded b)
	 (cond ((< value (lower-boundary b)) (lower-boundary b))
	       ((> value (upper-boundary b)) (upper-boundary b))
	       (t value)))
	((is-wrapped b)
	 (cond ((< value (lower-boundary b)) (upper-boundary b))
	       ((> value (upper-boundary b)) (lower-boundary b))
	       (t value)))
	(t value)))

(defmethod apply-self ((b brownian-motion) events &key)
  (mapc #'(lambda (event)
	    (let* ((current-value (gethash (event-source event) (lastval b)))
		   (new-value (cap b (+ current-value (* (nth (random 2) '(-1 1)) (step-size b))))))	      	      (setf (gethash (event-source event) (lastval b)) new-value)
	      (setf (slot-value event (modified-property b)) new-value))) events))






