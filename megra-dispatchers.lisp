(require 'incudine)

(load "megra-event-processors")

(defclass dispatcher ()
  ((perform-dispatch)
   (handle-events)
   (handle-transition)))

(defmethod perform-dispatch ((d dispatcher) (e event-processor) time &key)
  (when (is-active e)  
    (fresh-line)
    (handle-events d (pull-events e))
    (force-output)
    (let ((next (+ time (* 50 (handle-transition d (pull-transition e))))))
      (incudine:at next #'perform-dispatch d e next))))

					; dummy for testing and development 
(defclass string-dispatcher (dispatcher) ())

(defmethod handle-events ((s string-dispatcher) events &key)
  (fresh-line)
  (princ "the following events should be handled: ")
  (mapc #'(lambda (event)
	    (princ (event-message event))
	    (princ " from ")
	    (princ (event-source event))
	    (princ ", ")) events))

(defmethod handle-transition ((s string-dispatcher) (tr transition) &key)
  (fresh-line)
  (princ "the next events should happen in: ")
  (princ (transition-duration tr))
  (transition-duration tr))	 
