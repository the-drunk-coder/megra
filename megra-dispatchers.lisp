(require 'incudine)

(load "megra-event-processors")

(defclass dispatcher ()
  ((dispatch)
   (handle-events)
   (handle-transition)))

(defmethod dispatch ((d dispatcher) (e event-processor) time &key)
  (fresh-line)
  (handle-events d (pull-events e))
  (force-output)
  (if (is-active e)
  (let ((next (+ time (* 50 (handle-transition d (pull-transition e))))))
    (incudine:at next #'dispatch d e next))))


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
