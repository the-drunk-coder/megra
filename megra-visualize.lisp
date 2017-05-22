;;(in-package :megra)
(defparameter *max-label-length* 30)

;; land of lisp substrate ... 
(defun dot-name (exp)
  (substitute-if #\_ (complement #'alphanumericp) (prin1-to-string exp)))

(defun dot-label (exp)
  (if exp
      (let ((s (write-to-string exp :pretty nil)))
	(if (> (length s) *max-label-length*)
	    (concatenate 'string (subseq s 0 (- *max-label-length* 3))
			 "...")
	    s))
      ""))

(defmethod node->dot ((n node) &key)
  (format nil "~a[label=\"~a\"]~%"
	  (dot-name (node-id n))
	  (dot-name (node-id n))))

;;(node->dot (node 1 (mid 34)))
(in-package :megra)
(defmethod edge->dot ((e edge) &key)
  (format nil "~a->~a[weight=~a, penwidth=~a, rank=same]~%"
	  (edge-source e)
	  (edge-destination e)
	  ;;(transition-duration (car (edge-content e)))
	  (coerce (/ (edge-probablity e) 100) 'float)
	  (coerce (/ (edge-probablity e) 10) 'float)))

;;(edge->dot (edge 1 2 :prob 9 :dur 20))

(defmethod graph->dot ((g graph) &key)
  (format nil "digraph{~%~{~a~}~{~a~}}"
	  (loop for key being the hash-keys of (graph-nodes g)
	     collect (node->dot (gethash key (graph-nodes g))))
	  (loop for key being the hash-keys of (graph-edges g)
	       append (mapcar #'edge->dot (gethash key (graph-edges g))))))

;;(graph 'test ()
;;  (node 1 (mid 'a2 :tags '(blue)))
;;  (node 2 (mid 'a3))
;;  (edge 1 2 :dur 512 :prob 50)
;;  (edge 1 1 :dur 512 :prob 50)
;;  (edge 2 1 :dur 512 :prob 100))

;;(graph->dot (source-graph (gethash 'test *processor-directory*)))

