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

(defmethod node->dot ((n node) &key (output nil))
  (format output "~a[label=\"~a\"]~%"
	  ;;(dot-name (node-id n))
	  ;;(dot-name (node-id n))
	  (node-id n)
	  (node-id n)))

;;(node->dot (node 1 (mid 34)))
;;(in-package :megra)
(defmethod edge->dot ((e edge) &key (output nil))
  (format output "~a->~a[weight=~a, penwidth=~a, rank=same, arrowsize=~a]~%"
	  (edge-source e)
	  (edge-destination e)
	  ;;(transition-duration (car (edge-content e)))
	  (coerce (/ (edge-probablity e) 100) 'float)
	  (coerce (/ (edge-probablity e) 10) 'float)
	  (cond 
	    ((> (edge-probablity e) 90) 1)
	    ((> (edge-probablity e) 40) 0.5)
	    ((> (edge-probablity e) 1) 0.1)
	    (t 0))))

;;(edge->dot (edge 1 2 :prob 0 :dur 20))

(defmethod graph->dot ((g graph) &key (output nil))
  (format output "digraph{~%")
  (loop for val being the hash-values of (graph-nodes g)
     do (node->dot val :output output))
  (loop for val being the hash-values of (graph-edges g)
     do (mapc #'(lambda (edge) (edge->dot edge :output output)) val))
  (format output "~%}"))

;;(graph 'test ()
;;  (node 1 (mid 'a2 :tags '(blue)))
;;  (node 2 (mid 'a3))
;;  (edge 1 2 :dur 512 :prob 50)
;;  (edge 1 1 :dur 512 :prob 50)
;;  (edge 2 1 :dur 512 :prob 100))

;;(graph->dot (source-graph (gethash 'test *processor-directory*)))

