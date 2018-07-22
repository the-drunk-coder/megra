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
   (update-clones :accessor update-clones :initarg :update-clones :initform nil)))

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
   (path) ;; path is a predefined path - an idea i never picked up again ..
   (node-steps :accessor node-steps) ;; count how often each node has been evaluated ...
   (traced-path :accessor traced-path :initform nil) ;; trace the last events
   ;; length of the trace ...
   (trace-length :accessor trace-length :initarg :trace-length
		 :initform *global-trace-length*)))

;; This macro is basically just a wrapper for the (original) function,
;; so that i can mix keyword arguments and an arbitrary number of
;; ensuing graph elements ... 
(defmacro graph (name (&key
		       (perma nil) ;; what's this ???
		       (combine-mode ''append)
		       (affect-transition nil)
		       (combine-filter #'all-p)
		       (update-clones t) ;; what's this ???
		       (rand 0))
		 &body graphdata)
  `(funcall #'(lambda () (let ((new-graph (make-instance 'graph)))		      
		      (setf (graph-id new-graph) ,name)    
		      (mapc #'(lambda (obj)
				(cond ((typep obj 'edge) (insert-edge new-graph obj))
				      ((typep obj 'node) (insert-node new-graph obj))))
			    (list ,@graphdata))		      
		      ;; add random blind edges ...
		      (if (> ,rand 0) (randomize-edges new-graph ,rand))  
		      (if (gethash ,name *processor-directory*)
			  ;; update existing instance
			  (let ((cur-instance (gethash ,name *processor-directory*)))
			    (setf (source-graph cur-instance) new-graph)
			    (setf (affect-transition cur-instance) ,affect-transition)
			    (setf (combine-mode cur-instance) ,combine-mode)
			    (setf (combine-filter cur-instance) ,combine-filter)
			    (setf (update-clones cur-instance) ,update-clones)
			    (setf (copy-events cur-instance) (not ,perma))
			    (when ,update-clones
			      (mapc #'(lambda (proc-id)
					(let ((my-clone
					       (gethash proc-id *processor-directory*)))
					  (setf (source-graph my-clone)
						(deepcopy new-graph))
					  (setf (affect-transition my-clone) ,affect-transition)
					  (setf (combine-mode my-clone) ,combine-mode)
					  (setf (combine-filter my-clone) ,combine-filter)
					  (setf (update-clones my-clone) ,update-clones)
					  (setf (copy-events my-clone) (not ,perma))))
				    (clones cur-instance)))
			    cur-instance)			    
			  (setf (gethash ,name *processor-directory*)
				(make-instance 'graph-event-processor :name ,name
					       :graph new-graph :copy-events (not ,perma)
					       :current-node 1 :combine-mode ,combine-mode
					       :affect-transition ,affect-transition
					       :combine-filter ,combine-filter
					       :update-clones ,update-clones)))))))

;;  shorthand for graph
(setf (macro-function 'g) (macro-function 'graph))

;; replace the content (or parts of the content) of a graph ...
(defun graph-add (name new-content)
  (let ((current-graph (source-graph (gethash name *processor-directory*))))
    (mapc #'(lambda (obj)
	      (cond ((typep obj 'edge) (insert-edge current-graph obj))
		    ((typep obj 'node) (insert-node current-graph obj))))
	  new-content)
    (setf (source-graph (gethash name *processor-directory*)) current-graph)))

(defun graph-add-direct (graph new-content)  
  (mapc #'(lambda (obj)
	    (cond ((typep obj 'edge) (insert-edge graph obj))
		  ((typep obj 'node) (insert-node graph obj))))
	new-content))

;; clone a graph event processor ... 
(defun clone (original-id clone-id &key (variance 0.0) (track t) (store t) functors)
  (let ((original (gethash original-id *processor-directory*)))
    (when original
      (let ((clone (deepcopy original :imprecision variance :functors functors)))
	(when (typep original 'graph-event-processor)
	  (update-graph-name (source-graph clone) clone-id))	
	(setf (name clone) clone-id)
	(setf (chain-bound clone) nil)	
	(when store
	  (setf (gethash clone-id *processor-directory*) clone))
	(when track
	  (unless (member clone-id (clones original))
	    (setf (clones original) (append (clones original) (list clone-id)))))
	clone))))

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
			     (print-node
			      (gethash key (graph-nodes (source-graph g))))))	  
	  (loop for order being the hash-keys of (graph-outgoing-edges (source-graph g))
	     append (let ((order-edges (gethash order (graph-outgoing-edges (source-graph g)))))
		      (loop for key being the hash-keys of order-edges
			 append (mapcar
				 #'(lambda (edge) (format nil "~C~a~%" #\tab (print-edge edge)))
				 (gethash key order-edges)))))))

;; output helpers ... 
;; should i find a better name for this function ??
(defun pring (graph &optional stream)
  (format stream "~a" (print-graph (gethash graph *processor-directory*))))

(defun graph->code (graph file)
  (with-open-file (out-stream file :direction :output :if-exists :supersede)
    (format out-stream "~a" (print-graph (gethash graph *processor-directory*)))))

(defun graph->svg (graph file &key (renderer 'circo))
  (with-open-file (out-stream file :direction :output :if-exists :supersede)
    (graph->dot (source-graph (gethash graph *processor-directory*)) :output out-stream))
  (cond ((eq renderer 'dot)
	 (sb-ext:run-program "/usr/bin/dot" (list "-T" "svg" "-O" file "-Gnslimit" "-Gnslimit1")))
	((eq renderer 'neato)
	 (sb-ext:run-program "/usr/bin/neato" (list "-T" "svg" "-O"
						    file
						    "-Goverlap=scalexy -Gnodesep=0.6 -Gstart=0.5" )))
	((eq renderer 'circo)
	 (sb-ext:run-program "/usr/bin/circo" (list "-T" "svg" "-O" file)))
	((eq renderer 'sfdp)
	 (sb-ext:run-program "/usr/bin/sfdp" (list "-T" "svg" "-O" file
						   "-Goverlap=scalexy -Gnodesep=0.6 -Gstart=0.5")))
	((eq renderer 'twopi)
	 (sb-ext:run-program "/usr/bin/twopi" (list "-T" "svg" "-O" file "-Goverlap=scalexy")))))

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
    (setf (traced-path g) (append (traced-path g) (list (current-node g))))
    (when (> (list-length (traced-path g)) (trace-length g))
      (setf (traced-path g)
	    (delete (car (traced-path g)) (traced-path g) :count 1))))
  (if (copy-events g)
      (mapcar #'copy-instance
	      (node-content (gethash (current-node g)
				     (graph-nodes (source-graph g)))))
      (node-content (gethash (current-node g) (graph-nodes (source-graph g))))))

(defun match-trace (path pattern)
	 (let ((ldiff (- (length path) (length pattern))))
	   (if (>= ldiff 0)
	       (equal (nthcdr ldiff path) pattern))))

;; get the transition and set next current node ...
(defmethod current-transition ((g graph-event-processor) &key)
  (labels
      ((choice-list (edge counter)
	 (loop repeat (edge-probability edge)
	    collect counter))
       (collect-choices (edges counter)
	 (if edges
	     (append (choice-list (car edges) counter)
		     (collect-choices (cdr edges) (1+ counter)))
	     '())))
    ;; prioritize higher-order edges ...
    ;; this loop construction is creepy ...
    (loop named order-loop for order from
	 (graph-highest-edge-order (source-graph g)) downto 1
       ;; iterate over the edge orders ...
       if (gethash order (graph-outgoing-edges (source-graph g)))
       do (let ((edges-for-order (gethash order (graph-outgoing-edges (source-graph g))))) 
	    ;;(incudine::msg info "edge order: ~D" order)
	    (loop for pattern being the hash-keys of edges-for-order
	       ;; now, not only single nodes but also paths can serve as "source"
	       do (when (match-trace (traced-path g) pattern)
		    ;;(incudine::msg info "found edge ! pattern ~D" pattern)
		    (let* ((current-edges (gethash pattern edges-for-order))
			   (current-choices (collect-choices current-edges 0))
			   (chosen-edge-id (nth (random (length current-choices))
						current-choices))
			   (chosen-edge (nth chosen-edge-id current-edges)))
		      ;;(incudine::msg info "current-choices ~D" current-choices)
		      ;;(incudine::msg info "chosen edge id ~D" chosen-edge-id)
		      ;;(incudine::msg info "possible edges ~D" current-edges)
		      ;;(incudine::msg info "found edge ~D" chosen-edge)
		      (setf (current-node g) (edge-destination chosen-edge))
		      ;; if a valid transition has been found, jump out ... 
		      (if (copy-events g)
			  (return-from order-loop
			    (mapcar #'copy-instance (edge-content chosen-edge)))
			  (return-from order-loop (edge-content chosen-edge))
			  ))))))))

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

(defmethod filter-events ((m modifying-event-processor) events
			  &key (check-mod-prop t))
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

(defun stream-oscillate-between (name param upper-boundary lower-boundary &key cycle type
									    (affect-transition nil)
									    (keep-state t)
									    (track-state t)
									    (filter #'all-p)
									    (store nil))
  (let ((new-inst (make-instance 'stream-oscillate-between :mod-prop param :name name
				 :cycle cycle
				 :upper upper-boundary
				 :lower lower-boundary
				 :track-state track-state
				 :affect-transition affect-transition
				 :event-filter filter))
	(old-inst (gethash name *processor-directory*)))
    ;; if a current instance is replaced ...
    (when old-inst
      (setf (chain-bound new-inst) (chain-bound old-inst))
      (when keep-state
	(setf (pmod-step new-inst) (pmod-step (gethash name *processor-directory*)))
	(setf (lastval new-inst) (lastval (gethash name *processor-directory*)))))
    (when store
      (setf (gethash name *processor-directory*) new-inst))
    new-inst))

;; shorthand 
(defun s-oscb (param upper-boundary lower-boundary &key (cyc 128) 
						     (at nil)
						     (keep-state t)
						     (track-state t)
						     (f #'all-p)
						     (store nil)
						     (id nil))  
  (let ((s-osc-name (if id
			id
			(gensym))))
    (stream-oscillate-between s-osc-name param upper-boundary lower-boundary
			      :cycle cyc
			      :affect-transition at
			      :keep-state keep-state
			      :track-state track-state
			      :filter f
			      :store store
			      )))

(defclass chance-combine (modifying-event-processor)
  ((event-to-combine :accessor event-to-combine :initarg :event-to-combine)
   (combi-chance :accessor combi-chance :initarg :combi-chance)
   (step :accessor pmod-step :initform 0)))

;; make combi-chance accessible to live modifications
;; the create-accessor only works within a macro ... pretty hacky, all in all ... 
(eval (create-accessor 'chance-combine 'combi-chance 'combi-chance))

;; make state-tracking switchable ???
(defmethod apply-self ((c chance-combine) events &key)
  (mapcar #'(lambda (event)	    
	    (let ((chance-val (random 100)))
	      (if (and (funcall (event-filter c) event)
		       (< chance-val (combi-chance c)))		  
		  (combine-single-events (event-to-combine c) event)		  
		  event)))        
	events))

;; the constructor ... if store is set, it'll be stored in the processor directory,
;; if not, it'll be stored in the processor directory by the chain building
;; routine
(defun chance-combine (name chance event &key (affect-transition nil) (filter #'all-p) (store nil))
  (let ((new-inst (make-instance 'chance-combine
				 :name name
				 :combi-chance chance
				 :event-to-combine event
				 :track-state nil
				 :mod-prop nil
				 :affect-transition affect-transition
				 :event-filter filter))
	(old-inst (gethash name *processor-directory*)))
    (when old-inst
      (setf (chain-bound new-inst) (chain-bound old-inst)))
    (when store
      (setf (gethash name *processor-directory*) new-inst))
    new-inst))

;; shorthand
(defun cc (chance event &key (at nil) (f #'all-p) (id nil))
  (let ((cc-name (if id
		     id
		     (gensym))))
    ;;(incudine::msg info "~D" cc-name)
    (chance-combine cc-name chance event :affect-transition at :filter f)))

;; a random walk on whatever parameter ...
(defclass stream-brownian-motion
    (modifying-event-processor generic-brownian-motion) ())
  
;; make state-tracking switchable ??? 
(defmethod apply-self ((b stream-brownian-motion) events &key)
  (mapc #'(lambda (event)	    
	    (let* ((current-value (get-current-value b event))
		   (new-value (cap b
				   (+ current-value
				      (* (nth (random 2) '(-1 1))
					 (pmod-step-size b))))))
	      (setf (gethash (event-source event) (lastval b)) new-value)
	      (setf (slot-value event (modified-property b)) new-value)))
	(filter-events b events))
  events)

;; modifying ... always check if the modifier is already present !
(defun stream-brownian-motion (name param &key step-size wrap limit ubound lbound
					    (affect-transition nil) (keep-state t)
					    (track-state t) (filter #'all-p) (store nil))
  (let ((new-inst (make-instance 'stream-brownian-motion :step-size step-size :mod-prop param
				 :name name
				 :upper ubound
				 :lower lbound
				 :is-bounded limit
				 :is-wrapped wrap
				 :track-state track-state
				 :affect-transition affect-transition
				 :event-filter filter))
	(old-inst (gethash name *processor-directory*)))    
    (when old-inst
      (setf (chain-bound new-inst) (chain-bound old-inst))
      (when keep-state
	(setf (lastval new-inst) (lastval (gethash name *processor-directory*)))))
    (when store
      (setf (gethash name *processor-directory*) new-inst))
    new-inst))

;; shorthand
(defun s-brow (param lower upper &key (step 0.01)
					(wrap t)
					(limit nil) 
					(at nil)
					(keep-state t)
					(track-state t)
					(f #'all-p)
					(store nil)
					(id nil))
  (let ((s-brow-name (if id
			 id
			 (gensym))))
    (stream-brownian-motion s-brow-name param
			    :step-size step
			    :wrap wrap
			    :limit limit
			    :ubound upper
			    :lbound lower
			    :affect-transition at
			    :filter f
			    :track-state track-state
			    :keep-state keep-state
			    :store store)))

(defclass freeze-growth (modifying-event-processor)
  ((act :accessor freeze-growth-act :initarg :act)))

(defun freeze (act)
  (make-instance 'freeze-growth :act act
		 :name (gensym)
		 :mod-prop nil		 
		 :affect-transition nil
		 :event-filter nil) )

;; make state-tracking switchable ??? 
(defmethod apply-self ((f freeze-growth) events &key)
  (if (freeze-growth-act f)
      (remove-if #'(lambda (event) (or (typep event 'growth-event)
			      (typep event 'shrink-event)
			      (typep event 'population-control-event)))
		 events)
      events))

(defclass population-control (modifying-event-processor)
  ((variance :accessor population-control-var :initarg :variance)
   (pgrowth :accessor population-control-pgrowth :initarg :pgrowth)
   (pprune :accessor population-control-pprune :initarg :pprune)
   (method :accessor population-control-method :initarg :method)
   (durs :accessor population-control-durs :initarg :durs)
   (phoe :accessor population-control-higher-order-probability :initarg :phoe)
   (hoe-max :accessor population-control-higher-order-max-order :initarg :hoe-max)
   (exclude :accessor population-control-exclude :initarg :exclude)))

(defun popctrl (variance pgrowth pprune method
		&key durs (hoe 4) (hoe-max 4) exclude)
  (make-instance 'population-control
		 :name (gensym)
		 :mod-prop nil		 
		 :affect-transition nil
		 :event-filter nil
		 :variance variance
		 :pgrowth pgrowth
		 :pprune pprune
		 :method method
		 :durs durs
		 :phoe hoe
		 :hoe-max hoe-max
		 :exclude exclude))

(defmethod apply-self ((g population-control) events &key)
  (let ((ev-src (car (event-source (car events)))))
    (when (< (random 100) (population-control-pgrowth g))
      (let ((order (if (< (random 100)
			  (population-control-higher-order-probability g))
		       (+ 2 (random
			     (- (population-control-higher-order-max-order g) 2)))
		       nil)))
	;; append growth event
	(push (growth
	       ev-src
	       (population-control-var g)
	       :durs (population-control-durs g)
	       :method (population-control-method g)
	       :higher-order order)
	      events)))		  
    (when (< (random 100) (population-control-pprune g))
      ;; append prune event
      (push (shrink
	     ev-src					 
	     :exclude (population-control-exclude g))	    
	    events))
    events))
(in-package :megra)

(defclass parameter-limiter (modifying-event-processor)
  ((upper :accessor limiter-upper-limit :initarg :upper)
   (lower :accessor limiter-lower-limit :initarg :lower)))

(defun lim (param lower upper &key (f #'all-p))
  (make-instance 'parameter-limiter
		 :name (gensym)
		 :mod-prop param		 
		 :affect-transition nil
		 :event-filter f
		 :upper upper
		 :lower lower))

(defmethod apply-self ((p parameter-limiter) events &key)
  (mapc #'(lambda (event)	    
	    (let* ((current-value (get-current-value p event)))	      
	      (setf (slot-value event (modified-property p))
		    (cond ((< current-value (limiter-lower-limit p)) (limiter-lower-limit p))
			  ((> current-value (limiter-upper-limit p)) (limiter-upper-limit p))
			  (t current-value)))))
	(filter-events p events))
  events)

(defclass parameter-wrapper (modifying-event-processor)
  ((upper :accessor wrapper-upper-limit :initarg :upper)
   (lower :accessor wrapper-lower-limit :initarg :lower)))

(defun wrap (param lower upper &key (f #'all-p))
  (make-instance 'paramter-wrapper
		 :name (gensym)
		 :mod-prop param		 
		 :affect-transition nil
		 :event-filter f
		 :upper upper
		 :lower lower))

(defmethod apply-self ((w parameter-wrapper) events &key)
  (mapc #'(lambda (event)	    
	    (let* ((current-value (get-current-value w event)))	      
	      (setf (slot-value event (modified-property w))
		    (cond ((< current-value (wrapper-lower-limit w)) (wrapper-upper-limit w))
			  ((> current-value (wrapper-upper-limit w)) (wrapper-lower-limit w))
			  (t current-value)))))
	(filter-events w events))
  events)


(defclass processor-chain (event-processor)
  ((topmost-processor :accessor topmost-processor :initarg :topmost)
   (synced-chains :accessor synced-chains :initform nil)
   (synced-progns :accessor synced-progns :initform nil)
   ;; think of anschluss-zug -> connection train ... 
   (anschluss-kette :accessor anschluss-kette :initform nil) 
   (wait-for-sync :accessor wait-for-sync :initform nil)
   (active :accessor is-active :initform nil :initarg :is-active)
   (shift :accessor chain-shift :initform 0.0 :initarg :shift)
   (group :accessor chain-group :initform nil :initarg :group)))

(defun activate (chain)
  (incudine::msg info "activating ~D" chain)
  (setf (is-active chain) t))

;; deactivate ... if it's a modifying event processor, delete it ...
(defun deactivate (chain)
  (incudine::msg info "deactivating ~D" chain)
  (setf (is-active chain) nil))

(defmethod pull-events ((p processor-chain) &key)
  (pull-events (topmost-processor p)))

(defmethod pull-transition ((p processor-chain) &key)
  (pull-transition (topmost-processor p)))

(defun connect (processor-ids last chain-name unique)
  (let ((current (car processor-ids))
	(next (cadr processor-ids)))
    ;; if you try to hook it into a different chain ... 
    (if (and unique
	     next 
	     (chain-bound next)
	     (not (eql (chain-bound next) chain-name)))
	(progn
	  (incudine::msg
	   error
	   "cannot connect to ~D, already bound ..."
	   (cadr processor-ids))
	  ;; revert the work that has been done so far ... 
	  (detach current))      
	(when next
	  ;; if processor already has predecessor, it means that it is already
	  ;; bound in a chain ... 		
	  (setf (successor current) next)
	  (setf (predecessor next) current)	  
	  (connect (cdr processor-ids) (car processor-ids) chain-name unique)))
    ;;(incudine::msg
    ;;	   error
    ;;	   "fails hjer ?? ~D ~D" current chain-name)
    (setf (chain-bound current) chain-name)))

(defun gen-proc-name (ch-name proc idx)
  (intern (concatenate 'string
		       (string ch-name) "-"
		       (string (class-name (class-of proc))) "-"
		       (format nil "~d" idx))))

;; handle the processor list ...
(defun gen-proc-list (ch-name proc-list)
  (let ((idx 0))
    (mapcar #'(lambda (proc)
		(incf idx)	        
		(cond ((typep proc 'symbol)
		       (gethash proc *processor-directory*))
		      ;; check if proc is already present,
		      ;; if not, name it and insert it
		      ;; the proc constructor will check if
		      ;; there's
		      ;; an old instance of itself,
		      ;; and replace itself in that case
		      ((and (not (typep proc 'graph-event-processor))
			    (not (gethash (name proc) *processor-directory*)))
		       (let ((proc-name (gen-proc-name ch-name proc idx)))
			 (setf (name proc) proc-name)
			 (setf (gethash proc-name *processor-directory*) proc)))
		      ((typep proc 'graph-event-processor) proc)
		      ))
	    proc-list)))

(defmacro chain (name (&key (unique t) (activate nil) (shift 0.0) (group nil)) &body proc-body)
  `(funcall #'(lambda ()
		(let ((event-processors
		       (gen-proc-list ,name (list ,@proc-body))))
		(chain-from-list
		 ,name
		 event-processors
		 :unique ,unique
		 :activate ,activate
		 :shift ,shift
		 :group ,group)))))

;; if no group is given, the current group will be used ... 
(defun assign-chain-to-group (chain chain-name group)
  ;; if no groupname is given, use current group ... 
  (let* ((groupname (if group group *current-group*))
	 (group-list (gethash groupname *group-directory*)))
    (when (not (member chain-name group-list))
      (setf (chain-group chain) group)
      (setf (gethash groupname *group-directory*)
	      (append group-list (list chain-name))))))

(defmethod collect-chain ((c processor-chain) &key)
  (labels ((append-next (proc-list proc)	     
	     (if (successor proc)
		 (append-next (append proc-list (list proc))  (successor proc))
		 (append proc-list (list proc)))))
    (append-next '() (topmost-processor c))))

;; to change - push branch to regular chain directory,
;; just store the name in branch list.
;; that should allow for independent growth of branches ! 
(defun chain-from-list (name event-processors &key (unique t)
						(activate nil)
						(shift 0.0)
						(branch nil)
						(group nil))  
  (connect event-processors nil name unique)
  ;; assume the chaining went well 
  (let ((topmost-proc (car event-processors)))
    (if (chain-bound topmost-proc)
	(let ((new-chain (make-instance
			  'processor-chain
			  :topmost topmost-proc
			  :is-active activate
			  :shift shift)))	  
	  ;; assign chain to a group
	  (assign-chain-to-group new-chain name group)
	  ;; handle branching ...	  
	  (if branch
	      (setf (gethash branch *branch-directory*)
		    (append (gethash branch *branch-directory*) (list name))))
	  (setf (gethash name *chain-directory*) new-chain))
	(incudine::msg error "chain-building went wrong, seemingly ..."))))





