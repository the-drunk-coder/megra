(in-package :megra)

(defun pad-path (path)
  (let ((pathlength (length path)))
    (if (< pathlength *global-trace-length*)	
	(loop repeat (- *global-trace-length* pathlength)
	   do (push (car path) path))))
  path)

(defmethod post-growth-op ((g graph)
			   path
			   new-id
			   rebalance-nodes
			   higher-order
			   rnd
			   new-dur)
  (unless (> rnd 0)
    (rebalance-nodes g rebalance-nodes))
  (when higher-order
    (add-higher-order-edge g path higher-order new-id new-dur))
  (when (> rnd 0)
    (randomize-edges g rnd)
    (rebalance-edges g)))

;; idea:
;; pick node from path
;; this is, of course, only one possbility to grow a graph ... 
(defmethod grow-graph ((g graph-event-processor) &key (var 0)
						   durs
						   functors
						   rnd
						   higher-order)
  (let* ((path (pad-path (traced-path g))) ;; get the trace ...
	 (source-id (car path))
	 (reverse-path (reverse path))
	 (dest-id (car reverse-path))
	 (node-id (nth (random (length path)) path)) ;; pick a node id 
	 (picked-node (gethash node-id (graph-nodes (source-graph g))))	 
	 (new-id (+ (graph-max-id (source-graph g)) 1))
	 (new-node (make-instance 'node :id new-id :content nil :color 'white))
	 (new-dur (if durs
		      (nth (random (length durs)) durs)
		      (transition-duration
		       (car (edge-content
			     (get-edge (source-graph g)
				       (list (cadr reverse-path))
				       (car reverse-path))))))))
    ;; inject new content, with some variation 
    (setf (node-content new-node)
	  (deepcopy-list (node-content picked-node)
			 :imprecision var
			 :functors functors))
    ;; insert the new node
    (insert-node (source-graph g) new-node)
    ;; now, what to do about the edges ??
    ;; for now, build a bridge ??
    ;; a distance parameter might be nice, like, distance from cloned node ?
    ;; at zero, the new node would be only to the cloned node,
    ;; at far, the new node would be connected to far-away nodes ... 
    ;; THIS IS JUST FOR TESTING !! 
    (insert-edge (source-graph g)
		 (edge new-id source-id :dur new-dur :prob (+ 5 (random 20))))
    (insert-edge (source-graph g)
		 (edge source-id new-id :dur new-dur :prob (+ 5(random 20))))
    (insert-edge (source-graph g)
		 (edge new-id dest-id :dur new-dur :prob (+ 5 (random 20))))
    (insert-edge (source-graph g)
		 (edge dest-id new-id :dur new-dur :prob (+ 5 (random 20))))
    (post-growth-op (source-graph g)
		    path
		    new-id
		    (list new-id source-id dest-id)
		    higher-order
		    rnd
		    new-dur)))

;; grow graph in a fashion that becomes a tournament graph ... 
;;(defmethod grow-graph-tournament ((g graph-event-processor) &key (var 0) durs functors))

;; grow graph in a fashion that becomes a tournament graph ... 
;;(defmethod grow-graph-complete ((g graph-event-processor) &key (var 0) durs functors))

;; Grow graph in a fashion that becomes a number of interlocking three-event loops ... 
(defmethod grow-graph-triloop ((g graph-event-processor) &key (var 0)
							   durs
							   functors
							   rnd
							   higher-order)
  (let* ((path (pad-path (traced-path g))) ;; get the trace ...
	 (reverse-path (reverse path))
	 (source-id (car reverse-path))	 
	 (dest-id (cadr reverse-path))
	 (node-id (nth (random (length path)) path)) ;; pick a node id 
	 (picked-node (gethash node-id (graph-nodes (source-graph g))))	 
	 (new-id (+ (graph-max-id (source-graph g)) 1))
	 (new-node (make-instance 'node :id new-id :content nil :color 'white))
	 (new-dur (if durs
		      (nth (random (length durs)) durs)
		      (transition-duration
		       (car (edge-content
			     (get-edge (source-graph g)
				       (list (cadr reverse-path))
				       (car reverse-path))))))))
    ;;(incudine::msg error "new-id: ~D ~%" new-id)
    ;; inject new content, with some variation 
    (setf (node-content new-node)
	  (deepcopy-list (node-content picked-node)
			 :imprecision var
			 :functors functors))
    ;; insert the new node
    (insert-node (source-graph g) new-node)
    (insert-edge (source-graph g)
		 (edge new-id dest-id :dur new-dur :prob 100))
    (insert-edge (source-graph g)
		 (edge source-id new-id :dur new-dur :prob 100))
    (remove-edge (source-graph g) source-id dest-id)
    (post-growth-op (source-graph g)
		    path
		    new-id
		    (list source-id)
		    higher-order
		    rnd
		    new-dur)))

;; Grow graph in a fashion that becomes a number of interlocking three-event loops ... 
(defmethod grow-graph-quadloop ((g graph-event-processor) &key (var 0)
							    durs
							    functors
							    rnd
							    higher-order)
  (let* ((path (pad-path (traced-path g))) ;; get the trace ...
	 (reverse-path (reverse path))
	 (source-id (car reverse-path))
	 (dest-id (caddr reverse-path))
	 (node-id (nth (random (length path)) path)) ;; pick a node id 
	 (picked-node (gethash node-id (graph-nodes (source-graph g))))	 
	 (new-id (+ (graph-max-id (source-graph g)) 1))
	 (new-node (make-instance 'node :id new-id :content nil :color 'white))
	 (new-dur (if durs
		      (nth (random (length durs)) durs)
		      (transition-duration
		       (car (edge-content
			     (get-edge (source-graph g)
				       (list (cadr reverse-path))
				       (car reverse-path))))))))
    ;;(incudine::msg error "new-id: ~D ~%" new-id)
    ;; inject new content, with some variation 
    (setf (node-content new-node)
	  (deepcopy-list (node-content picked-node)
			 :imprecision var
			 :functors functors))
    ;; insert the new node
    (insert-node (source-graph g) new-node)
    (insert-edge (source-graph g)
		 (edge new-id dest-id :dur new-dur :prob 100))
    (insert-edge (source-graph g)
		 (edge source-id new-id :dur new-dur :prob 100))
    (remove-edge (source-graph g) source-id dest-id)
    (post-growth-op (source-graph g)
		    path
		    new-id
		    (list source-id)
		    higher-order
		    rnd
		    new-dur)))


(defmethod grow-graph-loop ((g graph-event-processor) &key
							(var 0)
							durs
							functors
							rnd
							higher-order)
  (let* ((path (pad-path (traced-path g))) ;; get the trace ...
	 (reverse-path (reverse path))
	 (dest-id (car reverse-path))	 
	 (source-id (cadr reverse-path))
	 (node-id (nth (random (length path)) path)) ;; pick a node id 
	 (picked-node (gethash node-id (graph-nodes (source-graph g))))	 
	 (new-id (+ (graph-max-id (source-graph g)) 1))
	 (new-node (make-instance 'node :id new-id :content nil :color 'white))
	 (new-dur (if durs
		      (nth (random (length durs)) durs)
		      (transition-duration
		       (car (edge-content
			     (get-edge (source-graph g)
				       (list (cadr reverse-path))
				       (car reverse-path))))))))    
    (setf (node-content new-node)
	  (deepcopy-list (node-content picked-node)
			 :imprecision var
			 :functors functors))
    ;; insert the new node
    (insert-node (source-graph g) new-node)
    (insert-edge (source-graph g)
		 (edge new-id dest-id :dur new-dur :prob 100))
    (insert-edge (source-graph g)
		 (edge source-id new-id :dur new-dur :prob 100))
    (remove-edge (source-graph g) source-id dest-id)
    (post-growth-op (source-graph g)
		    path
		    new-id
		    (list source-id)
		    higher-order
		    rnd
		    new-dur)))

(defun get-n-elements (list n)
  (loop for i from 0 to (- n 1)
       collect (nth i list)))

(defun get-last-n-elements (list n)
  (let ((rev (reverse list)))
    (reverse (loop for i from 0 to (- n 1)
       collect (nth i rev)))))

(defmethod add-higher-order-edge ((g graph)
				  path
				  order
				  new-node-id
				  dur &key)
  (let* ((source-path (get-last-n-elements path order)))
    (insert-edge g
		 (edge source-path new-node-id :dur dur :prob 100))))

(defun remove-all (items seq)
  (let ((first-removed (remove (car items) seq)))
    (if (and (cdr items) first-removed)
        (remove-all (cdr items) first-removed)
	first-removed)))

(defmethod prune-graph ((g graph-event-processor) &key exclude)
  (let* ((path (traced-path g)) ;; get the trace ...
	 (exclude-with-current (append exclude (list (current-node g))))
	 (reduced-path (remove-all exclude-with-current path)))
    (when reduced-path
      (let* ((prune-id (car (last reduced-path)))) ;; node id to remove ..	 
	;;(incudine::msg error "pruning ~D ~%"  prune-id)
	(setf (traced-path g) (remove prune-id (traced-path g)))	
	(remove-node (source-graph g) prune-id :rebalance t)))))

(defun grow (graph-id &key (variance 0)		        
			durs
			functors
			(method 'old)
			(rnd 0)
			higher-order)
  ;;(incudine::msg info "growing graph ~D" graph-id) 
  (cond ((eql method 'triloop)
	 (grow-graph-triloop (gethash graph-id *processor-directory*)
		     :var variance
		     :durs durs
		     :functors functors
		     :rnd rnd
		     :higher-order higher-order))
	((eql method 'quadloop)
	 (grow-graph-quadloop (gethash graph-id *processor-directory*)
		     :var variance
		     :durs durs
		     :functors functors
		     :rnd rnd
		     :higher-order higher-order))
	((eql method 'loop)
	 (grow-graph-loop (gethash graph-id *processor-directory*)
		     :var variance
		     :durs durs
		     :functors functors
		     :rnd rnd
		     :higher-order higher-order))
	(t (grow-graph (gethash graph-id *processor-directory*)
		     :var variance
		     :durs durs
		     :functors functors
		     :rnd rnd
		     :higher-order higher-order))))

(defun prune (graph-id &key exclude)
  ;;(incudine::msg info "pruning graph ~D" graph-id) 
  (prune-graph (gethash graph-id *processor-directory*)
	       :exclude exclude))

(defun branch (chain-id &key (shift 0) (variance 0.1) sync-to functors)
  (incudine::msg info "branching chain ~D" chain-id)			 
  ;; get the old chain ... 
  (let* ((current-chain (gethash chain-id *chain-directory*))
	 (current-procs (collect-chain current-chain))
	 (shift-diff (max 0 (- shift (chain-shift current-chain))))
	 (new-chain-id (gensym (symbol-name chain-id)))
	 ;; in contrast to the dispatcher branch method,
	 ;; here the new chain is pushed to the branch stack ... 
	 (new-chain (chain-from-list new-chain-id
				     (mapcar #'(lambda (proc)
						 (clone
						  (name proc)
						  (gensym (symbol-name
							   (name proc)))
						  :variance variance
						  :functors functors
						  :track nil))
					     current-procs)
				     :activate nil
				     :shift shift-diff
				     :group (chain-group current-chain)
				     :branch chain-id)))    
    ;;(incudine::msg error "start branch" )
    (inner-dispatch new-chain sync-to)))

