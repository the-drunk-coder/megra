;; event dispatchers and related stuff ... 
(defclass dispatcher ()
  ((step-dispatch)
   (perform-dispatch)
   (handle-events)
   (handle-transition)))

;; simple time-recursive dispatching
;; not using local variable binding to reduce consing (??)
(in-package :megra)

(defmethod perform-dispatch ((d dispatcher) proc time &key)
  (let ((event-processor (gethash proc *processor-directory*)))
    (when (and event-processor (is-active event-processor))
      ;; here, the events are produced and handled ...
      (loop for synced-proc in (synced-processors event-processor)
	 ;; don't check if it's active, as only deactivated procs are added to sync list
	 do (let ((sync-d (make-instance 'event-dispatcher)))
	      (format t "~a" synced-proc)
	      (activate synced-proc)
	      (perform-dispatch sync-d synced-proc (incudine:now))))
      (setf (synced-processors event-processor) nil)
      (handle-events d (pull-events event-processor))
      ;; here, the transition time between events is determinend,
      ;; and the next evaluation is scheduled ...
      (let* ((trans-time (handle-transition d (car
					       (pull-transition
						event-processor))))
	     (next (+ time #[trans-time ms])))
	(incudine:at next #'perform-dispatch d proc next)))))

;; manual step-by step dispatching ...
(defmethod step-dispatch ((d dispatcher) proc &key)
  (when (and (gethash proc *processor-directory*) (is-active (gethash proc *processor-directory*)) )
    (handle-events d (pull-events (gethash proc *processor-directory*)))
    (handle-transition d (car (pull-transition (gethash proc *processor-directory*))))))

(defmethod handle-transition ((s dispatcher) (tr transition) &key)
  (transition-duration tr))	 

;; dummy dispatcher for testing and development
(defclass string-dispatcher (dispatcher) ())

(defmethod handle-events ((s string-dispatcher) events &key)
  (fresh-line)
  (princ "the following events should be handled: ")
  (mapc #'(lambda (event)
	    (princ (event-message event))
	    (princ " from ")
	    (princ (event-source event))
	    (princ ", ")) events))

;; the main event dispatcher
(defclass event-dispatcher (dispatcher) ())

(defmethod handle-events ((e event-dispatcher) events &key)
  (mapc #'handle-event events))

