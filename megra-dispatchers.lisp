;; event dispatchers and related stuff ... 

(defclass dispatcher ()
  ((perform-dispatch)
   (handle-events)
   (handle-transition)))

(defmethod perform-dispatch ((d dispatcher) proc time &key)
  (let ((current-processor (gethash proc *processor-directory*)))
  (when (is-active current-processor) 
    (handle-events d (pull-events current-processor))
    (force-output)
    (let* ((trans-time (handle-transition d (pull-transition current-processor)))
	   (next (+ time #[trans-time ms])))
      (incudine:at next #'perform-dispatch d proc next)))))

(defmethod handle-transition ((s dispatcher) (tr transition) &key)
  (fresh-line)
  ;;(princ "the next events should happen in: ")
  ;;(princ (transition-duration tr))
  (transition-duration tr))	 

;; dummy dispatcher for testing and development
(defclass string-dispatcher (dispatcher) ())

(defmethod handle-events ((s string-dispatcher) events &key)
  (fresh-line)
  (princ "the following events should be handled: ")
  (mapc #'(lambda (event)
	    (princ (msg event))
	    (princ " from ")
	    (princ (event-source event))
	    (princ ", ")) events))

;; the main event dispatcher
(defclass event-dispatcher (dispatcher) ())

(defmethod handle-events ((e event-dispatcher) events &key)
  (mapc #'handle-event events))

;; handler methods for individual events ... 
(defmethod handle-event ((m midi-event) &key)
  (events (new midi
	       :time 0
	       :keynum (pitch m)
	       :duration (dur m)
	       :amplitude (round (* 127 (lvl m))))
	     :at (incudine:now)))
