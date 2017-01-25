(require 'incudine)

(defparameter *processor-directory* (make-hash-table :test 'eql))
(defparameter *dispatcher-directory* (make-hash-table :test 'eql))

(defun init-megra ()
  (incudine:rt-start)
  (sleep 1)
  (midi-open-default :direction :input)
  (midi-open-default :direction :output)
  ;(osc-open-default :host "127.0.0.1" :port 3002 :direction :input)
  ;(osc-open-default :host "127.0.0.1" :port 3003 :direction :output)
  ;(fudi-open-default :host "127.0.0.1" :port 3011 :direction :input)
  ;(fudi-open-default :host "127.0.0.1" :port 3012 :direction :output)
  (setf *out* (new incudine-stream))
  (setf *rts-out* *out*))

(in-package :common-lisp-user)

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
    (if (gethash name *processor-directory*)
	(setf (source-graph (get-processor name)) new-graph)
	(setf (get-processor name) (make-instance 'graph-event-processor :graph new-graph :current-node 1))))
  name)

					; dispatching
(defun dispatch (&rest event-processors)
  (let ((dispatcher (make-instance 'event-dispatcher)))
    (labels
	((connect (processors)
	   (when (cadr processors)
	     (setf (successor (get-processor (car processors))) (get-processor (cadr processors)) )
	     (connect (cdr processors)))))
      (connect event-processors))
    (perform-dispatch dispatcher (car event-processors) (incudine:now))))

					; events
(defun string-event (msg)
  (make-instance 'string-event :msg msg))

(defun midi (pitch &key dur lvl)
  (make-instance 'midi-event :pitch pitch :level lvl :duration dur))

					; miscellaneous
(defun deactivate (event-processor-id)
  (setf (is-active (get-processor event-processor-id)) NIL))


(defun activate (event-processor-id)
  (setf (is-active (get-processor event-processor-id)) t))

(defmacro get-processor (processor-id)
  `(gethash ,processor-id *processor-directory*))
