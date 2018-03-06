(in-package :megra)

;; idea:
;; pick node from path
;; this is, of course, only one possbility to grow a graph ... 
(defmethod grow-graph ((g graph-event-processor) &key (var 0) durs)
  (let* ((path (traced-path g)) ;; get the trace ...
	 (source-id (car path))
	 (dest-id (car (reverse path)))
	 (node-id (nth (random (length path)) path)) ;; pick a node id 
	 (picked-node (gethash node-id (graph-nodes (source-graph g))))	 
	 (new-id (+ (graph-max-id (source-graph g)) 1))
	 (new-node (make-instance 'node :id new-id :content nil :color 'white))
	 (new-dur (if durs
		      (nth (random (length durs)) durs)
		      (transition-duration
		       (car (edge-content
			     (get-edge (source-graph g)
				       (list (car  path))
				       (cadr path))))))))
    ;; inject new content, with some variation 
    (setf (node-content new-node)
	  (loop for object in (node-content picked-node)
	     collect (clone-imprecise object var)))
    ;; insert the new node
    (insert-node (source-graph g) new-node)
    ;; now, what to do about the edges ??
    ;; for now, build a bridge ??
    ;; a distance parameter might be nice, like, distance from cloned node ?
    ;; at zero, the new node would be only to the cloned node,
    ;; at far, the new node would be connected to far-away nodes ... 
    ;; THIS IS JUST FOR TESTING !! 
    (insert-edge (source-graph g) (edge new-id source-id :dur new-dur :prob 50))
    (insert-edge (source-graph g) (edge source-id new-id :dur new-dur :prob 20))
    (insert-edge (source-graph g) (edge new-id dest-id :dur new-dur :prob 50))
    (insert-edge (source-graph g) (edge dest-id new-id :dur new-dur :prob 20))))

(defun grow (graph-id &key (variance 0) durs)
  (grow-graph (gethash graph-id *processor-directory*) :var variance :durs durs))
