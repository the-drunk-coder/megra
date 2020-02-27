(in-package :megra)

;; graph-based event-generator, the main one ...
(defclass graph-event-processor (event-processor)
  ((source-graph :accessor source-graph :initarg :graph)
   (current-node :accessor current-node :initarg :current-node)   
   (combine-filter :accessor combine-filter :initarg :combine-filter)
   (affect-transition :accessor affect-transition :initarg :affect-transition)
   (node-steps :accessor node-steps) ;; count how often each node has been evaluated ...
   (traced-path :accessor traced-path :initform nil) ;; trace the last events
   ;; length of the trace ...
   (trace-length :accessor trace-length :initarg :trace-length :initform *global-trace-length*)))

(defmethod set-current-node ((w graph-event-processor) cnode &key)
  (setf (current-node w) cnode))

(defmethod set-traced-path ((w graph-event-processor) tpath &key)
  (setf (traced-path w) tpath))

;; This macro is basically just a wrapper for the (original) function,
;; so that i can mix keyword arguments and an arbitrary number of
;; ensuing graph elements ... 
(defmacro graph (name (&key		       
		         (combine-mode ''append)
		         (affect-transition nil)
		         (combine-filter #'all-p)		         
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
			    cur-instance)			    
			  (setf (gethash ,name *processor-directory*)
				(make-instance 'graph-event-processor :name ,name
					                              :graph new-graph 
					                              :current-node 1 :combine-mode ,combine-mode
					                              :affect-transition ,affect-transition
					                              :combine-filter ,combine-filter)))))))

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

;; turn back to textual representation ...
(defmethod print-graph ((g graph-event-processor) &key (out-stream nil))
  (format out-stream "(graph '~a (:combine-mode '~a :combine-filter #'~a)~%~{~a~}~{~a~})"
	  (graph-id (source-graph g))
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

;; get the current events as a copy, so that the originals won't change
;; as the events are pumped through the modifier chains ...
(defmethod current-events ((g graph-event-processor) &key)
  ;; append to trace
  (when (> (trace-length g) 0)
    (setf (traced-path g) (append (traced-path g) (list (current-node g))))
    (when (> (list-length (traced-path g)) (trace-length g))
      (setf (traced-path g)
	    (delete (car (traced-path g)) (traced-path g) :count 1))))
  (incf (node-age (gethash (current-node g) (graph-nodes (source-graph g)))))
  (mapcar #'copy-instance (node-content (gethash (current-node g) (graph-nodes (source-graph g))))))

(defun match-trace (path pattern)
  (let ((ldiff (- (length path) (length pattern))))
    (if (>= ldiff 0)
	(equal (nthcdr ldiff path) pattern))))

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
			    (return-from order-loop (mapcar #'copy-instance (edge-content chosen-edge))))))))))
