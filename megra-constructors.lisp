(require 'incudine)
					; structural
(defun node (id &rest content)
  (make-instance 'node :id id :content content))

(defun edge (src dest &key prob dur)
  (make-instance 'edge :src src :dest dest :prob prob :content `(,(make-instance 'transition :dur dur))))

(defun graph (name &rest graphdata)
  (let ((new-graph (make-instance 'graph)))
    (setf (graph-id new-graph) name)
    (mapc #'(lambda (obj)
	      (cond ((typep obj 'edge) (insert-edge new-graph obj))
		    ((typep obj 'node) (insert-node new-graph obj))))
	  graphdata)
    (make-instance 'graph-event-processor :graph new-graph :current-node 1)))

					; dispatching
(defun dispatch (dispatcher &rest event-processors)
  (labels
      ((connect (processors)
	 (if (cadr processors)
	     (setf (successor (car processors)) (cadr processors)))))
   (connect event-processors))
  (perform-dispatch dispatcher (car event-processors) (incudine:now)))



					; events
(defun string-event (msg)
  (make-instance 'string-event :msg msg))
