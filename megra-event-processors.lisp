;; generic event-processor
(defclass event-processor ()
  ((pull-events)
   (pull-transition)       
   (active :accessor is-active :initform nil)
   (successor :accessor successor :initform nil)
   (predecessor :accessor predecessor :initform nil)
   (has-successor)
   (has-predecessor)
   (current-events)      ;; abstract
   (current-transition)  ;; abstract   
   (name :accessor name :initarg :name)
   ))

(defmethod has-successor ((e event-processor) &key)
  (successor e))

(defmethod has-predecessor ((e event-processor) &key)
  (predecessor e))

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

;; pass -- default 
(defmethod current-transition ((m event-processor) &key))

;; dummy processor for testing, development and debugging ..
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

;; graph-based event-generator, the main one ...
(defclass graph-event-processor (event-processor) 
  ((source-graph :accessor source-graph :initarg :graph)
   (current-node :accessor current-node :initarg :current-node)
   (copy-events :accessor copy-events :initarg :copy-events :initform t)
   (combine-mode :accessor combine-mode :initarg :combine-mode)
   (combine-filter :accessor combine-filter :initarg :combine-filter)
   (path)))

;; strange mop-method to allow cloning events
;; eventually the event-sources are not considered,
;; but this shouldn't pose a problem so far ... 
(defmethod copy-instance (object)
   (let ((copy (allocate-instance (class-of object))))
     (loop for slot in (class-slots (class-of object))
	do (when (slot-boundp-using-class (class-of object) object slot)
	     (setf (slot-value copy (slot-definition-name slot))
		   (slot-value object (slot-definition-name slot)))))
     copy))

;; get the current events as a copy, so that the originals won't change
;; as the events are pumped through the modifier chains ...
(defmethod current-events ((g graph-event-processor) &key)
  (if (copy-events g)
      (mapcar #'copy-instance (node-content (gethash (current-node g) (graph-nodes (source-graph g)))))
      (node-content (gethash (current-node g) (graph-nodes (source-graph g))))))

;; get the transition and set next current node ...
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
    (if (copy-events g)
	(mapcar #'copy-instance (edge-content chosen-edge))
	(edge-content chosen-edge)))))

;; events are the successor events 
(defmethod apply-self ((g graph-event-processor) events &key)
  (combine-events (current-events g) events :mode (combine-mode g) :filter (combine-filter g)))

(defclass modifying-event-processor (event-processor)
  ((property :accessor modified-property :initarg :mod-prop)
   (last-values-by-source :accessor lastval)
   (affect-transition :accessor affect-transition :initarg :affect-transition)
   (track-state :accessor track-state :initarg :track-state :initform t)
   (event-filter :accessor event-filter :initarg :event-filter)))

(defmethod initialize-instance :after ((m modifying-event-processor) &key)
  (setf (lastval m) (make-hash-table :test 'eql)))

(defmethod pull-transition ((e modifying-event-processor) &key)
  (if (has-successor e)
      (progn
	(current-transition e)        
	(if (affect-transition e)
	    (apply-self e (pull-transition (successor e)))
	    (pull-transition (successor e))))
      (current-transition e)))

(defmethod apply-self :before ((m modifying-event-processor) events &key)
  ;; state tracking 
  (mapc #'(lambda (event)
	    (if (event-has-slot-by-name event (modified-property m))
		(unless (gethash (event-source event) (lastval m))  
		  (setf (gethash (event-source event) (lastval m))
			(slot-value event (modified-property m))))
		event))
	events))

(defmethod get-current-value ((m modifying-event-processor) (e event) &key)
  (if (track-state m)
      (gethash (event-source e) (lastval m))
      (slot-value e (modified-property m))))

(defmethod filter-events ((m modifying-event-processor) events &key (check-mod-prop t))
  (labels ((current-filter-p (event)
	     (if check-mod-prop
		 (and (event-has-slot-by-name event (modified-property m))
				 (funcall (event-filter m) event))
		 (funcall (event-filter m) event))))
    (remove-if-not #'current-filter-p events)))
;; switch to preserve/not preserve state ?

;; oscillate a parameter between different values ...
(defclass oscillate-between (modifying-event-processor)
  ((upper-boundary :accessor upper-boundary :initarg :upper-boundary)
   (lower-boundary :accessor lower-boundary :initarg :lower-boundary)   
   (cycle :accessor cycle :initarg :cycle )
   (step-count :accessor step-count :initform 0)
   (type :accessor osc-type :initarg :type)))

;; helper ...
(defun radians (numberOfDegrees) 
  (* pi (/ numberOfDegrees 180.0)))

(defmethod apply-self ((o oscillate-between) events &key)
  (mapc #'(lambda (event)
	    (let* ((current-value (get-current-value o event))
		       (osc-range (- (upper-boundary o) (lower-boundary o)))		   
		       (degree-increment (/ 360 (cycle o)))
		       (degree (mod (* degree-increment (mod (step-count o) (cycle o))) 360))
		       (abs-sin (abs (sin (radians degree))))		   
		       (new-value (+ (lower-boundary o) (* abs-sin osc-range))))
		  ;; this is basically the phase-offset
		  (setf (step-count o) (1+ (step-count o)))
		  (setf (gethash (event-source event) (lastval o)) new-value)	      
		  (setf (slot-value event (modified-property o)) new-value)))
	(filter-events o events))
  events)

;; special event processor for convenience purposes, to define
;; a persistent endpoint for a chain, i.e. when constantly modifying
;; the chain in the development process.
(defclass spigot (event-processor)
  ((flow :accessor flow :initarg :flow :initform t)))

(defmethod apply-self ((s spigot) events &key)
    (if (flow s)
	events
	'()))

(defclass chance-combine (modifying-event-processor)
  ((event-to-combine :accessor event-to-combine :initarg :event-to-combine)
   (combi-chance :accessor combi-chance :initarg :combi-chance)))

;; make state-tracking switchable ??? 
(defmethod apply-self ((c chance-combine) events &key)
  ;;(princ "c_combi_")
  (mapc #'(lambda (event)	    
	    (let ((chance-val (random 100)))
	      ;;(princ chance-val)
	      (if (< chance-val (combi-chance c))
		  (progn
		    ;;(princ "combi")
		    (combine-single-events (event-to-combine c) event))
		  event)))
	(filter-events c events :check-mod-prop nil))
  events)

;; a random walk on whatever parameter ...
(defclass brownian-motion (modifying-event-processor)
  ((upper-boundary :accessor ubound :initarg :upper-boundary)
   (lower-boundary :accessor lbound :initarg :lower-boundary)
   (step-size :accessor step-size :initarg :step-size)
   (is-bounded :accessor is-bounded :initarg :is-bounded)
   (is-wrapped :accessor is-wrapped :initarg :is-wrapped)))

;; cap or wrap ...
(defmethod cap ((b brownian-motion) value &key)
  (cond ((is-bounded b)
	 (cond ((< value (lbound b)) (lbound b))
	       ((> value (ubound b)) (ubound b))
	       (t value)))
	((is-wrapped b)
	 (cond ((< value (lbound b)) (ubound b))
	       ((> value (ubound b)) (lbound b))
	       (t value)))
	(t value)))

;; make state-tracking switchable ??? 
(defmethod apply-self ((b brownian-motion) events &key)
  (mapc #'(lambda (event)	    
	    (let* ((current-value (get-current-value b event))
		   (new-value (cap b (+ current-value
					(* (nth (random 2) '(-1 1)) (step-size b))))))
	      (setf (gethash (event-source event) (lastval b)) new-value)
	      (setf (slot-value event (modified-property b)) new-value)))
	(filter-events b events))
  events)








