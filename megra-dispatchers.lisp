(load "megra-event-processors")

(defclass dispatcher ()
  ((dispatch)
   (handle-events)
   (handle-transition)))

(defmethod dispatch ((d dispatcher) (e event-processor) &key)
  (handle-events d (pull-events e))
  (handle-transition d (pull-transition e)))

(defclass string-dispatcher (dispatcher) ())

(defmethod handle-events ((s string-dispatcher) events &key)
  (fresh-line)
  (princ "the following events should be handled: ")
  (mapc #'(lambda (event)
	    (princ (event-message event))
	    (princ " ")) events))

(defmethod handle-transition ((s string-dispatcher) (tr transition) &key)
  (fresh-line)
  (princ "the next events should happen in: ")
  (princ (transition-duration tr)))
