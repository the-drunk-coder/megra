(require 'incudine)
(require 'cm)

(load "megra-event-processors")
					; incudine/midi init
(in-package :cm)
(progn
  (incudine:rt-start)
  (sleep 1)
  (midi-open-default :direction :input)
  (midi-open-default :direction :output)
  (osc-open-default :host "127.0.0.1" :port 3002 :direction :input)
  (osc-open-default :host "127.0.0.1" :port 3003 :direction :output)
  (fudi-open-default :host "127.0.0.1" :port 3011 :direction :input)
  (fudi-open-default :host "127.0.0.1" :port 3012 :direction :output)
  (setf *out* (new incudine-stream))
  (setf *rts-out* *out*))

(in-package :common-lisp-user)

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

(defmethod handle-event ((m midi-event) &key)
  (cm:events (cm:new cm:midi
	       :time 0
	       :keynum (event-pitch m)
	       :duration (event-duration m)
	       :amplitude (round (* 127 (event-level m))))
	     :at (incudine:now)))

(defmethod handle-events ((e event-dispatcher) events &key)
  (mapc #'handle-event events))


