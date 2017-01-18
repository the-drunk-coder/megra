(require 'incudine)

(load "megra-event-processors")

(defclass dispatcher ()
  ((dispatch-2)
   (handle-events)
   (handle-transition)))

(defmethod dispatch ((d dispatcher) (e event-processor) &key)
  (fresh-line)
  (princ "DISPATCH")
  ;(handle-events d (pull-events e))
  (let ((next (+ (incudine:now) (handle-transition d (pull-transition e)))))
    (incudine:at next #'dispatch d e)))

(defmethod dispatch-2 ((d dispatcher) (e event-processor) now &key)
  (fresh-line)
  (princ "DISPATCH ")
  (let ((next (+ now 1000)))
    (princ next)
    (incudine:at next #'dispatch-2 d e next)))


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
