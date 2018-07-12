(in-package :megra)

;; idea:
;; pick node from path
;; this is, of course, only one possbility to grow a graph ... 
(defmethod grow-graph ((g graph-event-processor) &key (var 0) durs functors rnd)
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
    (when (> rnd 0) (randomize-edges (source-graph g) rnd))
    (rebalance-edges (source-graph g))))

;; grow graph in a fashion that becomes a tournament graph ... 
;;(defmethod grow-graph-tournament ((g graph-event-processor) &key (var 0) durs functors))

;; grow graph in a fashion that becomes a tournament graph ... 
;;(defmethod grow-graph-complete ((g graph-event-processor) &key (var 0) durs functors))

;; Grow graph in a fashion that becomes a number of interlocking three-event loops ... 
(defmethod grow-graph-triloop ((g graph-event-processor) &key (var 0)
							   durs functors rnd)
  (let* ((path (traced-path g)) ;; get the trace ...
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
    ;;(incudine::msg info "picked: ~D~%" (node-id picked-node))
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
    (when (> rnd 0) (randomize-edges (source-graph g) rnd))
    (rebalance-edges (source-graph g))))

;; Grow graph in a fashion that becomes a number of interlocking three-event loops ... 
(defmethod grow-graph-quadloop ((g graph-event-processor) &key (var 0)
							    durs functors rnd)
  (let* ((path (traced-path g)) ;; get the trace ...
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
    ;;(incudine::msg info "picked: ~D~%" (node-id picked-node))
    (incudine::msg info "source: ~D~%" source-id)
    (incudine::msg info "dest: ~D~%" dest-id)
    (incudine::msg info "revpath: ~D~%" reverse-path)  
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
    (when (> rnd 0) (randomize-edges (source-graph g) rnd))
    (rebalance-edges (source-graph g))))


(defmethod grow-graph-loop ((g graph-event-processor) &key (var 0)
							durs functors rnd)
  (let* ((path (traced-path g)) ;; get the trace ...
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
    ;;(incudine::msg info "source: ~D~%" source-id)
    ;;(incudine::msg info "dest: ~D~%" dest-id)
    ;;(incudine::msg info "revpath: ~D~%" reverse-path)
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
    (when (> rnd 0) (randomize-edges (source-graph g) rnd))
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
    ;;(format t "~D~%" (current-node g))
    (when reduced-path
      (let* ((prune-id (car (last reduced-path)))) ;; node id to remove ..	 
	(setf (traced-path g) (remove prune-id (traced-path g)))
	(remove-node (source-graph g) prune-id)))))

(defun grow (graph-id &key (variance 0)
			;;(growth-replication 10)
			;;(shrink-replication 20)
			durs
			functors
			(method 'old)
			(rnd 0))
  (incudine::msg info "growing graph ~D" graph-id) 
  (cond ((eql method 'triloop)
	 (grow-graph-triloop (gethash graph-id *processor-directory*)
		     :var variance
		     :durs durs
		     :functors functors
		     :rnd rnd))
	((eql method 'quadloop)
	 (grow-graph-quadloop (gethash graph-id *processor-directory*)
		     :var variance
		     :durs durs
		     :functors functors
		     :rnd rnd))
	((eql method 'loop)
	 (grow-graph-loop (gethash graph-id *processor-directory*)
		     :var variance
		     :durs durs
		     :functors functors
		     :rnd rnd))
	(t (grow-graph (gethash graph-id *processor-directory*)
		     :var variance
		     :durs durs
		     :functors functors
		     :rnd rnd))))

(defun prune (graph-id &key exclude durs)
  (incudine::msg info "pruning graph ~D" graph-id) 
  (prune-graph (gethash graph-id *processor-directory*)
	       :exclude exclude :durs durs))

(defun branch (chain-id &key (shift 0) (variance 0.1) sync-to functors)
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
						  :functors functors
						  :track nil))
					     current-procs)
				     :activate nil
				     :shift shift-diff
				     :group (chain-group current-chain)
				     :branch t)))
    ;;(incudine::msg error "start branch" )
    (inner-dispatch (car new-chain) sync-to)))

