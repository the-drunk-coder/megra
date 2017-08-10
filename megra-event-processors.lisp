;; generic event-processor
(defclass event-processor ()
  ((pull-events)
   (pull-transition)       
   (successor :accessor successor :initform nil)
   (predecessor :accessor predecessor :initform nil)   
   (current-events)      ;; abstract
   (current-transition)  ;; abstract   
   (chain-bound :accessor chain-bound :initform nil)   
   (name :accessor name :initarg :name)
   (clones :accessor clones :initform nil)
   (update-close :accessor update-clones :initarg :update-clones :initform nil)))

(defmethod pull-events ((e event-processor) &key)
  (if (successor e)
      (apply-self e (pull-events (successor e)))
      (current-events e)))

(defmethod pull-transition ((e event-processor) &key)
  (if (successor e)
      (progn
	(current-transition e) ;; trigger node selection ...
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
   (affect-transition :accessor affect-transition :initarg :affect-transition)
   (path) ;; path is a predefined path
   (node-steps :accessor node-steps) ;; count how often each node has been evaluated ...
   (traced-path :accessor traced-path :initform nil) ;; trace the last events
   ;; length of the trace ...
   (trace-length :accessor trace-length :initarg :trace-length :initform *global-trace-length*)))

;; turn back to textual representation ...
(defmethod print-graph ((g graph-event-processor) &key (out-stream nil))
  (format out-stream "(graph '~a (:perma ~a :combine-mode '~a :combine-filter #'~a)~%~{~a~}~{~a~})"
	  (graph-id (source-graph g))
	  (copy-events g)
	  (combine-mode g)
	  (print-function-name (combine-filter g))	 
	  ;; might save hashtable access here ... 
	  (loop for key being the hash-keys of (graph-nodes (source-graph g))
	     collect (format nil "~C~a~%"
			     #\tab
			     (print-node (gethash key (graph-nodes (source-graph g))))))	  
	  (loop for key being the hash-keys of
	       (graph-edges (source-graph g))
	     append
	       (mapcar #'(lambda (edge) (format nil "~C~a~%"
			    #\tab
			    (print-edge edge)))
		       (gethash key (graph-edges (source-graph g)))))))


;; initialize counter hash table ...
(defmethod initialize-instance :after ((g graph-event-processor) &key)
  (setf (node-steps g) (make-hash-table :test 'eql)))

;; strange mop-method to allow cloning events
;; eventually the event-sources are not considered,
;; but this shouldn't pose a problem so far ... 
(defmethod copy-instance (object)
   (let ((copy (allocate-instance (class-of object))))
     (loop for slot in (class-slots (class-of object))
	do (when (slot-boundp-using-class (class-of object) object slot)
	     (setf (slot-value copy (slot-definition-name slot))	   
		   ;; if told so, evaluate slots while copying ...
		   ;; should make some things easier ...
		   (if (and *eval-on-copy*
			    (not (member (slot-definition-name slot) *protected-slots*)))
		       (let ((val (slot-value object (slot-definition-name slot))))
			 (cond ((typep val 'param-mod-object) (evaluate val))
			       ((typep val 'function) (funcall val))
			       (t val)))		       
		       (slot-value object (slot-definition-name slot)))))) copy))

;; get the current events as a copy, so that the originals won't change
;; as the events are pumped through the modifier chains ...
(defmethod current-events ((g graph-event-processor) &key)
  ;; append to trace
  (when (> (trace-length g) 0)
    (setf (traced-path g) (nconc (traced-path g) (list (current-node g))))
    (when (> (list-length (traced-path g)) (trace-length g))
      (setf (traced-path g) (delete (car (traced-path g)) (traced-path g) :count 1))))
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

(defmethod apply-self-transition ((g graph-event-processor) current-transition transition &key)
  (combine-events current-transition transition :mode (combine-mode g) :filter (combine-filter g)))

(defmethod pull-transition ((g graph-event-processor) &key)
  (if (successor g)
      (let ((cur-trans (current-transition g)))
	(if (affect-transition g)
	    (apply-self-transition g cur-trans (pull-transition (successor g)))
	    (pull-transition (successor g))))
      (current-transition g)))


;; with the advent of param-mod-objects, some of these might be deemed deprecated,
;; but left for legacy reasons ...
(defclass modifying-event-processor (event-processor)
  ((property :accessor modified-property :initarg :mod-prop)
   (last-values-by-source :accessor lastval)
   (affect-transition :accessor affect-transition :initarg :affect-transition)
   (track-state :accessor track-state :initarg :track-state :initform t)
   (event-filter :accessor event-filter :initarg :event-filter)
   (step :accessor pmod-step :initform 0)))

(defmethod initialize-instance :after ((m modifying-event-processor) &key)
  (setf (lastval m) (make-hash-table :test 'eql)))

(defmethod pull-transition ((e modifying-event-processor) &key)
  (if (successor e)
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

(defmethod apply-self :after ((m modifying-event-processor) events &key)
  (setf (pmod-step m) (1+ (pmod-step m))))
	   
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


;; oscillate a parameter between different values in a stream
(defclass stream-oscillate-between (generic-oscillate-between modifying-event-processor) ())

;; helper ...
(defun radians (numberOfDegrees) 
  (* pi (/ numberOfDegrees 180.0)))

(defmethod apply-self ((o stream-oscillate-between) events &key)
  (mapc #'(lambda (event)
	    (let* ((osc-range (- (pmod-upper o) (pmod-lower o)))		   
		   (degree-increment (/ 360 (pmod-cycle o)))
		   (degree (mod (* degree-increment (mod (pmod-step o) (pmod-cycle o))) 360))
		   (abs-sin (abs (sin (radians degree))))		   
		   (new-value (+ (pmod-lower o) (* abs-sin osc-range))))
	      ;; this is basically the phase-offset	      
	      (setf (gethash (event-source event) (lastval o)) new-value)	      
	      (setf (slot-value event (modified-property o)) new-value)))
	(filter-events o events))
  events)

(defclass chance-combine (modifying-event-processor)
  ((event-to-combine :accessor event-to-combine :initarg :event-to-combine)
   (combi-chance :accessor combi-chance :initarg :combi-chance)
   (step :accessor pmod-step :initform 0)))

;; make state-tracking switchable ??? 
(defmethod apply-self ((c chance-combine) events &key)
  (mapc #'(lambda (event)	    
	    (let ((chance-val (random 100)))
	      (if (< chance-val (combi-chance c))
		  (progn
		    (combine-single-events (event-to-combine c) event))
		  event)))
	(filter-events c events :check-mod-prop nil))
  events)

;; a random walk on whatever parameter ...
(defclass stream-brownian-motion (modifying-event-processor generic-brownian-motion) ())
  
;; make state-tracking switchable ??? 
(defmethod apply-self ((b stream-brownian-motion) events &key)
  (mapc #'(lambda (event)	    
	    (let* ((current-value (get-current-value b event))
		   (new-value (cap b (+ current-value
					(* (nth (random 2) '(-1 1)) (pmod-step-size b))))))
	      (setf (gethash (event-source event) (lastval b)) new-value)
	      (setf (slot-value event (modified-property b)) new-value)))
	(filter-events b events))
  events)

(defclass processor-chain (event-processor)
  ((topmost-processor :accessor topmost-processor :initarg :topmost)
   (synced-chains :accessor synced-chains :initform nil)
   (active :accessor is-active :initform nil :initarg :is-active)))

(in-package :megra)
(defun activate (chain-id)
  (incudine::msg info "activating ~D" chain-id)
  (setf (is-active (gethash chain-id *chain-directory*)) t))

;; deactivate ... if it's a modifying event processor, delete it ... 
(defun deactivate (chain-id &key (del nil))
  (setf (is-active (gethash chain-id *chain-directory*)) nil))

(defmethod pull-events ((p processor-chain) &key)
  (pull-events (topmost-processor p)))

(defmethod pull-transition ((p processor-chain) &key)
  (pull-transition (topmost-processor p)))

(in-package :megra)
(defun connect (processor-ids last chain-name unique)
  (let ((current (gethash (car processor-ids) *processor-directory*))
	(next (gethash (cadr processor-ids) *processor-directory*))
	(last (gethash last *processor-directory*)))
    ;; if you try to hook it into a different chain ... 
    (if (and unique
	     (chain-bound current)
	     (not (eql (chain-bound current) chain-name)))
	(progn
	  (incudine::msg
	   error
	   "cannot connect ~D, already bound ..."
	   (car processor-ids))
	  ;; revert the work that has been done so far ... 
	  (detach last))      
	(when (cadr processor-ids)
	  ;; if processor already has predecessor, it means that it is already
	  ;; bound in a chain ... 		
	  (setf (successor current) next)
	  (setf (predecessor next) current)	  
	  (connect (cdr processor-ids) (car processor-ids) chain-name unique)))
    (setf (chain-bound current) chain-name)))

(defun detach (processor)
  (when (predecessor processor)
    (detach (predecessor processor) current-processor-ids)
    (setf (predecessor processor) nil))
  (when (successor processor)
    (setf (successor processor) nil))
  (setf (chain-bound processor) nil))

;; chain events without dispatching ...
(in-package :megra)
(defmacro chain (name (&key (unique t) (activate nil)) &body proc-body)
  `(funcall #'(lambda () (let ((event-processors (list ,@proc-body)))		    
		      (connect event-processors nil ,name ,unique)
		      ;; assume the chaining went well 
		      (let ((topmost-proc (gethash
					   (car event-processors)
					   *processor-directory*)))
			(if (chain-bound topmost-proc)
			    (setf (gethash ,name *chain-directory*)
				  (make-instance
				   'processor-chain
				   :topmost topmost-proc
				   :is-active ,activate))
			    (incudine::msg error "chain-building went wrong, seemingly ...")))))))





