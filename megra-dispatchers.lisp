;; event dispatching and related stuff ... 

;; simple time-recursive dispatching
;; not using local variable binding to reduce consing (??)
(in-package :megra)

(defun perform-dispatch (proc time)
  (let ((event-processor (gethash proc *processor-directory*)))    
    (when (and event-processor (is-active event-processor))
      ;; here, the events are produced and handled ...
      (loop for synced-proc in (synced-processors event-processor)
	 ;; don't check if it's active, as only deactivated procs are added to sync list
	 do (progn
	      (activate synced-proc)
	      (perform-dispatch synced-proc (incudine:now))))
      ;; reset all synced processors
      (setf (synced-processors event-processor) nil)      
      ;; handle events from current graph
      (handle-events (pull-events event-processor))
      ;; here, the transition time between events is determinend,
      ;; and the next evaluation is scheduled ...
      (let* ((trans-time (handle-transition (car
					     (pull-transition
					      event-processor))))
	     (next (+ time #[trans-time ms])))	
	;;(incudine:at next #'incudine:nrt-funcall #'(lambda () (perform-dispatch d proc next)))))))
	(incudine:at next #'perform-dispatch proc next)))))

;; manual step-by step dispatching ...
(defun step-dispatch (proc)
  (when (and (gethash proc *processor-directory*) (is-active (gethash proc *processor-directory*)) )
    (handle-events (pull-events (gethash proc *processor-directory*)))
    (handle-transition (car (pull-transition (gethash proc *processor-directory*))))))

(defun handle-transition (tr)
  (transition-duration tr))	 

;; dummy dispatcher for testing and development
;;(defclass string-dispatcher (dispatcher) ())

;;(defmethod handle-events ((s string-dispatcher) events &key)
;;  (fresh-line)
;;  (princ "the following events should be handled: ")
;;  (mapc #'(lambda (event)
;;	    (princ (event-message event))
;;	    (princ " from ")
;;	    (princ (event-source event))
;;          (princ ", ")) events))

;; the main event dispatcher
(defun handle-events (events)
  (mapc #'handle-event events))

