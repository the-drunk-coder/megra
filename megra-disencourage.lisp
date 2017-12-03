;; knuth shuffle, needed as helper ...
(defun shuffle-list (l)
  (loop for i from (- (list-length l) 1) downto 1
     do (let* ((current-elem-idx (random i))
	       (random-elem (nth current-elem-idx l)))	  
	  (setf (nth current-elem-idx l) (nth i l))
	  (setf (nth i l) random-elem)))
  ;; return shuffled list ... somewhat imperative, again .. 
  l)
(in-package :megra)
;; the heart of the disencourage algorithm ... 
(defmethod encourage-path ((g graph-event-processor) prob-mod &key)
  ;; the double reverse is performed to drop the last element, as this will be
  ;; not really percieved by the user, i guess ... 
  (loop for (src dest) on (reverse (cdr (reverse (traced-path g)))) while dest
     do (let* ((edge-source (if (typep src 'list) src (list src)))
	       (encouraged-edge (get-edge (source-graph g)
					  edge-source
					  dest))
	       (discouraged-edges
		(shuffle-list
		 (remove encouraged-edge
			 ;; get edges for order 1 ... 
			 (gethash edge-source
				  (gethash 1 (graph-edges (source-graph g)))))))
	       (ghost-edges nil)
	       (discourage-points prob-mod))
	  ;;(format t "encourage ~a ~a" src dest)
	  ;; the edge to encourage
	  (setf (edge-probablity encouraged-edge)
		(if (<=  (edge-probablity encouraged-edge) (- 100 prob-mod))
		    (setf (edge-probablity encouraged-edge)
			  (+ (edge-probablity encouraged-edge) prob-mod))
		    (setf (edge-probablity encouraged-edge) 100)))
	 (osc:message (osc-vis-out g)
		       "/set_edge_weight" "iif"
		       (edge-source encouraged-edge)
		       (edge-destination encouraged-edge)
		       (coerce (* (edge-probablity encouraged-edge) 0.25) 'float)
		       )
	 ;; distribute discouraging points
	 (loop while (and (> discourage-points 0)
			   (> (list-length discouraged-edges) 0))
	     do (let ((current-edge (car discouraged-edges)))		  
		  ;;(format t "discourage: ~a ~a ~%" (edge-source current-edge)
		  ;;	  (edge-destination current-edge) )
		  (cond ((<= (edge-probablity current-edge) 99)
			 (incf (edge-probablity current-edge))
			 (setf discourage-points (- discourage-points 1)))
			((>= (edge-probablity current-edge) 100)
			 ;; remove edge that has zero prob
			 (setf discouraged-edges
			       (remove current-edge discouraged-edges))
			 (setf ghost-edges
			       (append ghost-edges (list current-edge))))
			 ;; otherwise, rotate
			(t (setf discouraged-edges (append
						   (remove current-edge
							   discouraged-edges)
						   (list current-edge)))))))
	  (loop for edge in discouraged-edges
	     do (osc:message (osc-vis-out g)
			     "/set_edge_weight" "iif"
			     (edge-source edge)
			     (edge-destination edge)
			     (coerce (* (edge-probablity edge) 0.25) 'float)))
	  (loop for edge in ghost-edges
	     do (osc:message (osc-vis-out g)
			     "/set_edge_weight" "iif"
			     (edge-source edge)
			     (edge-destination edge)
			     (coerce (* (edge-probablity edge) 0.25) 'float)))
	  )))

(defmethod discourage-path ((g graph-event-processor) prob-mod &key)
  ;; the double reverse is performed to drop the last element, as this will be
  ;; not really percieved by the user, i guess ... 
  (loop for (src dest) on (reverse (cdr (reverse (traced-path g)))) while dest
     do (let* ((edge-source (if (typep src 'list) src (list src)))
	       (discouraged-edge (get-edge (source-graph g)
					   edge-source
					   dest))
	       (encouraged-edges
		(shuffle-list
		 (remove discouraged-edge
			 (gethash edge-source
				  (gethash 1 (graph-edges (source-graph g)))))))
	       (satiated-edges nil)
	       (encourage-points prob-mod))
	  ;;(format t "discourage ~a ~a" src dest)
	  ;; the edge to discourage
	  (setf (edge-probablity discouraged-edge)
		(if (>=  (edge-probablity discouraged-edge) prob-mod)
		    (setf (edge-probablity discouraged-edge)
			  (- (edge-probablity discouraged-edge) prob-mod))
		    (setf (edge-probablity discouraged-edge) 0)))
	  ;; notify visualizer
	  (osc:message (osc-vis-out g)
		       "/set_edge_weight" "iif"
		       (edge-source discouraged-edge)
		       (edge-destination discouraged-edge)
		       (coerce  (* (edge-probablity discouraged-edge) 0.25) 'float))
	  ;; distribute encouraging points
	  (loop while (and (> encourage-points 0)
			   (> (list-length encouraged-edges) 0))
	     do (let ((current-edge (car encouraged-edges)))		  
		  ;;(format t "encourage: ~a ~a ~%" (edge-source current-edge)
		  ;;	  (edge-destination current-edge) )
		  (cond ((<= (edge-probablity current-edge) 99)
			 (incf (edge-probablity current-edge))
			 (setf encourage-points (- encourage-points 1)))
			((>= (edge-probablity current-edge) 100)
			 ;; remove edge that has zero prob
			 (setf encouraged-edges
			       (remove current-edge encouraged-edges))
			 (setf satiated-edges
			       (append satiated-edges (list current-edge))))
			 ;; otherwise, rotate
			(t (setf encouraged-edges (append
						   (remove current-edge
							   encouraged-edges)
						   (list current-edge)))))))
	  (loop for edge in encouraged-edges
	     do (osc:message (osc-vis-out g)
			     "/set_edge_weight" "iif"
			     (edge-source edge)
			     (edge-destination edge)
			     (coerce (* (edge-probablity edge) 0.25) 'float)))
	  (loop for edge in satiated-edges
	     do (osc:message (osc-vis-out g)
			     "/set_edge_weight" "iif"
			     (edge-source edge)
			     (edge-destination edge)
			     (coerce (* (edge-probablity edge) 0.25) 'float))))))

;; encourage or discourage single graph event processor
(defun encourage (graph)
  (encourage-path (gethash graph *processor-directory*) *encourage-percentage*))

(defun discourage (graph)
  (discourage-path (gethash graph *processor-directory*) *discourage-percentage*))

;; encourage graph event processor and its successors
(defun encourage-with-tail (graph)
  (encourage-chain (gethash graph *processor-directory*)))

(defun discourage-with-tail (graph)
  (discourage-chain (gethash graph *processor-directory*)))

;; same as above, the internals 
(defun encourage-chain (proc)
  (if (typep proc 'graph-event-processor)
      (encourage-path proc *encourage-percentage*))
  (if (successor proc)
      (encourage-chain (successor proc))))

(defun discourage-chain (proc)
  (if (typep proc 'graph-event-processor)
      (discourage-path proc *discourage-percentage*))
  (if (successor proc)
      (discourage-chain (successor proc))))

(defun encourage-all ()
  (labels ((encourage-if-active-graph (key item)
	     (if (is-active item)
		 (encourage-chain item))))
    (maphash #'encourage-if-active-graph *processor-directory*)))

(defun discourage-all ()
  (labels ((discourage-if-active-graph (key item)
	     (if (is-active item)
		 (discourage-chain item))))
    (maphash #'discourage-if-active-graph *processor-directory*)))
