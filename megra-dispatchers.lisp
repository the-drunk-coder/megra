(require 'incudine)
(require 'cm)

					; need that ?
(load "megra-event-processors")
					; incudine/midi init

(defclass dispatcher ()
  ((perform-dispatch)
   (handle-events)
   (handle-transition)))

(defmethod perform-dispatch ((d dispatcher) proc time &key)
  (let ((current-processor (gethash proc *graph-directory*)))
  (when (is-active current-processor) 
    (handle-events d (pull-events current-processor))
    (force-output)
    (let ((next (+ time (* 50 (handle-transition d (pull-transition current-processor))))))
      (incudine:at next #'perform-dispatch d proc next)))))

(defmethod handle-transition ((s dispatcher) (tr transition) &key)
  (fresh-line)
  (princ "the next events should happen in: ")
  (princ (transition-duration tr))
  (transition-duration tr))	 

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

(defclass event-dispatcher (dispatcher) ())

(defmethod handle-events ((e event-dispatcher) events &key)
  (mapc #'handle-event events))

(in-package :cm)
					; handler methods for individual events ... 
(defmethod handle-event ((m midi-event) &key)
  (cm:events (cm:new cm:midi	       
	       :keynum (pitch m)
	       :duration (dur m)
	       :amplitude (round (* 127 (lvl m))))
	     :at (incudine:now)))


