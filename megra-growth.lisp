(in-package :megra)

;; idea:
;; pick node from path
;; this is, of course, only one possbility to grow a graph ... 
(defmethod grow-graph ((g graph-event-processor) &key (var 0) durs)
  (let* ((path (traced-path g)) ;; get the trace ...
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
    (incudine::msg info "picked: ~D~%" (node-id picked-node))
    ;; inject new content, with some variation 
    (setf (node-content new-node)
	  (loop for object in (node-content picked-node)
	     collect (deepcopy object :imprecision var)))
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
    (rebalance-edges (source-graph g))))

(defun remove-all (items seq)
  (let ((first-removed (remove (car items) seq)))
    (if (and (cdr items) first-removed)
        (remove-all (cdr items) first-removed)
	first-removed)))

(defmethod prune-graph ((g graph-event-processor) &key exclude durs)
  (let* ((path (traced-path g)) ;; get the trace ...
	 (exclude-with-current (append exclude (list (current-node g))))
	 (reduced-path (remove-all exclude-with-current path)))
    ;;(format t "~D~%" path)
    ;;(format t "~D~%" reduced-path)
    (when reduced-path
      (let* ((prune-id (car (last reduced-path))) ;; node id to remove ..
	     (prune-idx-in-path (position prune-id path :from-end t))
	     (source-id (if (eql prune-idx-in-path 0)
			    (get-first-inbound-edge-source
			     (source-graph g) prune-id)
			    (nth (- prune-idx-in-path 1) path)))
	     (dest-id (nth (+ prune-idx-in-path 1) path))
	     ;; old edge must exist, otherwise the path couldn't have
	     ;; happened like this ...
	     (old-edge (get-edge (source-graph g) (list source-id) prune-id))
	     (new-edge (get-edge (source-graph g) (list source-id) dest-id)))
	(incudine::msg info "removing node: ~D" prune-id)
	;; only when this edge doesn't exist ...
	(when (and (not new-edge) (not (eql source-id dest-id)))
	  (let ((new-dur (if durs
			     (nth (random (length durs)) durs)
			     (transition-duration (car (edge-content
							old-edge)))))
		(new-prob (edge-probability old-edge)))
	    (insert-edge (source-graph g)
			 (edge source-id dest-id :dur new-dur :prob new-prob))))
	;; finally, remove the node to be pruned !
	(setf (traced-path g) (remove prune-id (traced-path g)))
	(remove-node (source-graph g) prune-id)
	))))

(defun grow (graph-id &key (variance 0)
			(growth-replication 10)
			(shrink-replication 20)
			durs)
  (incudine::msg info "growing graph ~D" graph-id) 
  (grow-graph (gethash graph-id *processor-directory*) :var variance :durs durs))

(defun prune (graph-id &key exclude durs)
  (incudine::msg info "pruning graph ~D" graph-id) 
  (prune-graph (gethash graph-id *processor-directory*)
	       :exclude exclude :durs durs))

(defun branch (chain-id &key (shift 0) (variance 0.1) sync-to)
  (incudine::msg info "branching chain ~D" chain-id)			 
  ;; get the old chain ... 
  (let* ((current-chain (gethash chain-id *chain-directory*))
	 (current-procs (collect-chain current-chain))
	 (shift-diff (max 0 (- shift (chain-shift current-chain))))
	 ;; in contrast to the dispatcher branch method,
	 ;; here the new chain is pushed to the branch stack ... 
	 (new-chain (chain-from-list chain-id				     
				     (mapcar #'(lambda (proc)
						 (clone
						  (name proc)
						  (gensym (symbol-name
							   (name proc)))
						  :variance variance
						  :track nil))
					     current-procs)				     
				     :activate nil
				     :shift shift-diff
				     :group (chain-group current-chain)
				     :branch t)))
    ;;(incudine::msg error "start branch" )
    (inner-dispatch (car new-chain) sync-to)))

