;; the basic structure of music ...
(defclass node ()
  ((global-id :accessor node-global-id)
   (id :accessor node-id :initarg :id)
   (content :accessor node-content :initarg :content)
   (color :accessor node-color :initarg :color :initform 'white)
   (age :accessor node-age :initarg :age :initform 0))) ;; how many times has this node been evaluated ?
   
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
;; (see megra-events)
;; and placed in the edge content
(defclass edge ()
  (;; still named "source", but as the source can
   ;; also be a path, the naming is a little inaccurate
   (source :accessor edge-source :initarg :src)   
   (destination :accessor edge-destination :initarg :dest)
   (probability :accessor edge-probability :initarg :prob)
   (content :accessor edge-content :initarg :content)))

;; i could split transition print here, but i think it's ok like that for now ...
;; turn back to textual representation ...
(defmethod print-edge ((e edge) &key)
  (if (typep (edge-source e) 'list)
      (format nil "(edge '~D ~d :prob ~d :dur ~d)"
	      (edge-source e)
	      (edge-destination e)
	      (edge-probability e)
	      (transition-duration (car (edge-content e))))
      (format nil "(edge ~D ~d :prob ~d :dur ~d)"
	      (edge-source e)
	      (edge-destination e)
	      (edge-probability e)
	      (transition-duration (car (edge-content e))))))

;; compare edges to remove duplicates
(defmethod edge-equals ((a edge) (b edge) &key)
  (and (equal (edge-source a) (edge-source b))
       (eql (edge-destination a) (edge-destination b))))

;; everything can be organized as a graph, really ...
(defclass graph ()
  ((id :accessor graph-id)
   (nodes :accessor graph-nodes)
   (outgoing-edges :accessor graph-outgoing-edges)
   (incoming-edges :accessor graph-incoming-edges)
   (max-id :accessor graph-max-id :initform 0)
   (highest-edge-order :accessor graph-highest-edge-order :initform 0)))

(defmethod initialize-instance :after ((g graph) &key)
  (setf (graph-nodes g) (make-hash-table :test 'eql))
  (setf (graph-outgoing-edges g) (make-hash-table :test 'eql))
  (setf (graph-incoming-edges g) (make-hash-table :test 'eql)))

(defmethod random-node-id ((g graph) &key)
  (alexandria::hash-table-keys (graph-nodes g)))
  

(defmethod insert-node ((g graph) (n node) &key)
  (setf (node-global-id n) (list (graph-id g) (node-id n)))
  ;; set event source ids with format:
  ;; ((GRAPH-ID NODE-ID EVENT-POS) 
  ;; ----
  ;; keep track of highest id (meaning that ids must be sortable ... )
  (if (> (node-id n) (graph-max-id g))
      (setf (graph-max-id g) (node-id n)))
  (if (node-content n)
      (labels ((identify (events count)
		 (if events
		     (progn
		       (setf (event-source (car events))
			     (append (node-global-id n) (list count)))
		       (setf (event-tags (car events))
			     (remove-duplicates
			      (append (event-tags (car events))
				      (list (node-color n)))))
		       (identify (cdr events) (+ 1 count))))))
	(identify (node-content n) 0)))
  (setf (gethash (node-id n) (graph-nodes g)) n))

(in-package :megra)
(defun sum-probs (edge-list)
  (loop for e in edge-list
     summing (edge-probability e) into tprob
     finally (return tprob)))

;; rebalance the probabilies for a node (or higher-order source)
(defmethod rebalance-node ((g graph) node-id &key)
  (let* ((node-id-list (if (typep node-id 'list)
			   node-id
			   (list node-id)))
	 (order (length node-id-list))
	 (edge-list (gethash node-id-list (gethash order (graph-outgoing-edges g))))
	 (sprob (sum-probs edge-list))
	 (pdiff (- 100 sprob))
	 (num-edges (length edge-list)))    
    (cond ((> pdiff 0)	   
	   ;; IMPORTANT !! CHECK if edge pro is at 0 !! 
	   (loop while (> pdiff 0)
	      do (progn
		   (incf (edge-probability
			  (nth (random num-edges)
			       edge-list)))
		   (decf pdiff))))
	  ((< pdiff 0)	   
	   (let ((indices (loop for i from 0 to
			       (- num-edges 1) collect i)))
	     (loop while (or (< pdiff 0)
			     (eql 0 (length indices)))
		do (let ((chosen-idx
			  (nth (random (length indices)) indices)))
		     (if (>= (edge-probability
			      (nth chosen-idx edge-list)) 1)
			 (progn
			   (decf (edge-probability
				  (nth chosen-idx edge-list)))
			   (incf pdiff))
			 (remove chosen-idx indices)))))))))

(defmethod rebalance-nodes ((g graph) node-ids &key)
  (mapc #'(lambda (id) (rebalance-node g id)) node-ids))

;; idea: add exclusion list, so this method can be used for disencourage ??
(defmethod rebalance-edges ((g graph) &key)
  (loop for order being the hash-keys of (graph-outgoing-edges g)
     do (loop for src-edges being the hash-values of
	     (gethash order (graph-outgoing-edges g))
	   do (let* ((sprob (sum-probs src-edges))
		     (pdiff (- 100 sprob)))
		(cond ((> pdiff 0)
		       ;; IMPORTANT !! CHECK if edge pro is at 0 !! 
		       (loop while (> pdiff 0)
			  do (progn
			       (incf (edge-probability
				      (nth (random (length src-edges))
					   src-edges)))
			       (decf pdiff))))
		      ((< pdiff 0)
		       (let ((indices (loop for i from 0 to
					   (- (length src-edges) 1) collect i)))
			 (loop while (or (< pdiff 0)
					 (eql 0 (length indices)))
			    do (let ((chosen-idx
				      (nth (random (length indices)) indices)))
				 (if (>= (edge-probability
					  (nth chosen-idx src-edges)) 1)
				     (progn
				       (decf (edge-probability
					      (nth chosen-idx src-edges)))
				       (incf pdiff))
				     (remove chosen-idx indices)))))))))))

;; return lists of all nodes that have been "touched",
;; make new connections between lists if necessary ... 
;; parent and child isn't totally precise here as
;; the graphs aren't hierachical, but i didn't find
;; more intuitive names ... 
(defmethod collect-parent-ids-and-durations ((g graph) removed-id &key order)
  (let ((edges (gethash removed-id (gethash order (graph-incoming-edges g)))))
    (remove nil
	    (mapcar #'(lambda (edge)
			(unless (eql (edge-source edge) removed-id)
			  (list (edge-source edge)
				(transition-duration (car (edge-content edge))))))
		    edges))))

(defmethod collect-child-ids-and-durations ((g graph) removed-id &key order)
  (let ((edges (gethash removed-id (gethash order (graph-outgoing-edges g)))))
    (remove nil
	    (mapcar #'(lambda (edge)		        
			(unless (eql (edge-destination edge) (car removed-id))
			  (list (edge-destination edge)
				(transition-duration (car (edge-content edge))))))
		    edges))))

(defmethod remove-edges-containing-id ((g graph) id)
  ;; remove higher-order edges that contain the removed id
  (loop for order being the hash-keys of (graph-outgoing-edges g)
     do (loop for src-seq being the hash-keys of
	     (gethash order (graph-outgoing-edges g))
	   do (when (member id src-seq)
		(let ((edge-dests
		       (loop for edge in
			    (gethash src-seq
				     (gethash order (graph-outgoing-edges g)))
			  collect (edge-destination edge))))
		  (mapc #'(lambda (dest) (remove-edge g src-seq dest)) edge-dests))))))

(defmethod has-incoming-1st-order ((g graph) id &key)
  (< 0 (length (gethash id (gethash 1 (graph-incoming-edges g))))))

(defmethod has-outgoing-1st-order ((g graph) id &key)
  (< 0 (length (gethash id (gethash 1 (graph-outgoing-edges g))))))

(defmethod remove-node ((g graph) removed-id &key (rebalance nil))
  (loop for order being the hash-keys of (graph-outgoing-edges g)
     do (if (eql order 1)
	    (let ((involved-parents (collect-parent-ids-and-durations
				     g
				     removed-id
				     :order order))
		  (involved-children (collect-child-ids-and-durations
				      g
				      (list removed-id)
				      :order order)))
	      ;;(incudine::msg error "par ~D" involved-parents)
	      ;;(incudine::msg error "chi ~D" involved-children)
	      (mapc #'(lambda (par) (remove-edge g (car par) removed-id))
		    involved-parents)
	      (mapc #'(lambda (ch) (remove-edge g removed-id (car ch)))
		    involved-children)
	      ;; make a connection from every parent to every child,
	      ;; to keep graph connected
	      ;; (before, it was possible to go from any parent to any child
	      ;; via the removed node, so we want to keep that trait ...)
	      (loop for parent in involved-parents
		 do (loop for child in involved-children
		       do (progn
			    (insert-edge g (edge (car parent)
						 (car child)
						 :prob 100 :dur (cadr parent)))))))
	    ;; there are only incoming higher-order edges ...
	    (let ((involved-parents (collect-parent-ids-and-durations
				     g
				     removed-id
				     :order order)))
	      (mapc #'(lambda (par) (remove-edge g (car par) removed-id))
		    involved-parents))))
  ;; now, check if there's any higher order edge that contains
  ;; the removed node, and remove it ...
  ;;(incudine::msg error "rem cont hoe")
  (remove-edges-containing-id g removed-id)
  ;; remove node from registrer ...
  (remhash removed-id (graph-nodes g))
  ;; if specified, rebalance edges ... 
  (if rebalance
      (rebalance-edges g)))

;; fetch the first inbound edge of a node ... 
(defmethod get-first-inbound-edge-source ((g graph) node-id &key (order 1))
  (edge-source (car (gethash node-id  (gethash order (graph-incoming-edges g))))))

(defmethod insert-edge ((g graph) (e edge) &key)
  (let ((edge-source-list (if (typep (edge-source e) 'list)
			      (edge-source e)
			      (list (edge-source e))))
	(edge-dest (edge-destination e)))
    ;; set transition source ids with format:
    ;; ((GRAPH-ID . E) . (SOURCE . DEST)) 
    (if (edge-content e)
	(setf (event-source (car (edge-content e))) 
	      ;; might need to be adapted to accomodate lists 
	      (cons (cons (graph-id g) 'E) (cons (edge-source e)
						 (edge-destination e)))))
    (let ((edge-order (length edge-source-list)))
      ;; update highest edge order if necessary ... 
      (when (> edge-order (graph-highest-edge-order g))
	(setf (graph-highest-edge-order g) edge-order))
      ;; create outgoing edges table for order if not present
      (unless (gethash edge-order (graph-outgoing-edges g))	
	(setf (gethash edge-order (graph-outgoing-edges g))
	      (make-hash-table :test 'equal)))
      ;; create incoming edges table for order if not present
      (unless (gethash edge-order (graph-incoming-edges g))
	(setf (gethash edge-order (graph-incoming-edges g))
	      (make-hash-table :test 'equal)))     
      (let* ((order-dict-outgoing (gethash edge-order (graph-outgoing-edges g)))
	     (outgoing-edges (gethash edge-source-list order-dict-outgoing))
	     (order-dict-incoming (gethash edge-order (graph-incoming-edges g)))
	     (incoming-edges (gethash edge-dest order-dict-incoming)))
	;;(incudine::msg error "add edge, src ~D  dest ~D" (edge-source e) (edge-destination e))
	(setf (gethash edge-source-list order-dict-outgoing)
	      (remove-duplicates (cons e outgoing-edges) :test #'edge-equals))
	(setf (gethash edge-dest order-dict-incoming)
	      (remove-duplicates (cons e incoming-edges) :test #'edge-equals))))))

(defmethod remove-edge ((g graph) source destination &key)
  (let* ((edge-source-list (if (typep source 'list)
			       source
			       (list source)))
	 (real-source (if (and (typep source 'list) (eql (length source) 1))
			  (car source)
			  source))
	 (edge-order (length edge-source-list))
	 (order-dict-outgoing (gethash edge-order (graph-outgoing-edges g)))
	 (order-dict-incoming (gethash edge-order (graph-incoming-edges g)))
	 (source-edges (gethash edge-source-list order-dict-outgoing))
	 (dest-edges (gethash destination order-dict-incoming)))
    (setf (gethash edge-source-list order-dict-outgoing)
	  (remove (edge real-source destination :dur 0 :prob 0)
		  source-edges :test #'edge-equals))
    (setf (gethash destination order-dict-incoming)
	  (remove (edge real-source destination :dur 0 :prob 0)
		  dest-edges :test #'edge-equals))
    (when (not (gethash edge-source-list order-dict-outgoing))
      (remhash edge-source-list order-dict-outgoing))
    (when (not (gethash destination order-dict-incoming))
      (remhash destination order-dict-incoming))))

(defmethod graph-size ((g graph))
  (hash-table-count (graph-nodes g)))

(defmethod get-edge ((g graph) source destination)
  (labels ((find-destination (edges destination)
	     ;; might be more efficient to use edge list ordered by
	     ;; destination and use binary search ... 
	     (if (car edges)
		 (if (eql (edge-destination (car edges)) destination)
		     (car edges)
		     (find-destination (cdr edges) destination)))))
    (let ((current-edges (gethash source
				  (gethash (length source)
					   (graph-outgoing-edges g)))))
      (find-destination current-edges destination))))

;; helper function to add random edges to a graph ... 
(defmethod randomize-edges ((g graph) chance)
  (loop for src being the hash-keys of (graph-nodes g)
     do (loop for dest being the hash-keys of (graph-nodes g)
	   do (let ((randval (random 100)))
		(if (and (< randval chance)
			 (not
			  (get-edge g (if (typep src 'sequence)
					  src
					  (list src))
				    dest)))
		    (insert-edge g (edge src dest :prob 0)))))))

(defmethod update-graph-name ((g graph) new-name &key)
  (setf (graph-id g) new-name)
  (loop for n being the hash-values of (graph-nodes g)
     do (progn (setf (node-global-id n) (list new-name (node-id n)))
	       (loop for ev in (node-content n)
		  do (setf (nth 0 (event-source ev)) new-name)))))

;; regarding probability modification:
;; an event is pumped through the chain and each
;; processor knows the source of the event, as it is stored
;; in the event.
;; so we can modify the edge that led to that event,

;; functions and macros that serve as shorthands ... 
(defun node (id &rest content)
  (make-instance 'node :id id :content content :color 'white))

;; shorthand for node 
(defun n (id &rest content)
  (make-instance 'node :id id :content content :color 'white))

(defmacro node-col (id (&key (col ''white)) &body content)
  `(make-instance 'node :id ,id :content (list ,@content) :color ,col))

;; shorthand for node-col
(setf (macro-function 'n-c) (macro-function 'node-col))

(defun edge (src dest &key prob (dur 512))
  (make-instance 'edge :src src :dest dest :prob prob
		 :content `(,(make-instance 'transition :dur dur))))

;; shorthand for edge
(defun e (src dest &key p (d 512))
  (make-instance 'edge :src src :dest dest :prob p :content `(,(make-instance 'transition :dur d))))



