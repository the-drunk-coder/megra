;; event dispatchers and related stuff ... 

(defclass dispatcher ()
  ((perform-dispatch)
   (handle-events)
   (handle-transition)))

;; simple time-recursive dispatching
(defmethod perform-dispatch ((d dispatcher) proc time &key)

  (when (and (gethash proc *processor-directory*) (is-active (gethash proc *processor-directory*)) )
    (handle-events d (pull-events (gethash proc *processor-directory*)))
    (let* ((trans-time (handle-transition d (pull-transition (gethash proc *processor-directory*))))
	   (next (+ time #[trans-time ms])))
      (incudine:at next #'perform-dispatch d proc next))))

(defmethod handle-transition ((s dispatcher) (tr transition) &key)
  ;;(fresh-line)
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
  (events (cm::new cm::midi
	       :time 0
	       :keynum (pitch m)
	       :duration (dur m)
	       :amplitude (round (* 127 (lvl m))))
	  :at (incudine:now)))

(defmethod handle-event ((g grain-event) &key)
  (unless (gethash (sample-location g) *buffer-directory*)
    (let* ((buffer (incudine:buffer-load (sample-location g)))
	   (bdata (make-buffer-data :buffer buffer
				    :buffer-rate (/ (incudine:buffer-sample-rate buffer)
						    (incudine:buffer-frames buffer))
				    :buffer-frames (incudine:buffer-frames buffer))))
      (setf (gethash (sample-location g) *buffer-directory*) bdata)))
  (let ((bdata (gethash (sample-location g) *buffer-directory*)))
    (scratch::megra-grain (buffer-data-buffer bdata)
		 (buffer-data-buffer-rate bdata)
		 (buffer-data-buffer-frames bdata)
		 (lvl g)
		 (rate g)
		 (start g)
		 (lp-freq g)
		 (lp-q g)
		 (lp-dist g)
		 (pf-freq g)
		 (pf-q g)
		 (pf-gain g)
		 (hp-freq g)
		 (hp-q g)
		 (* (atk g) 0.001)
		 (* (- (dur g) (atk g) (rel g)) 0.001)
		 (* (rel g) 0.001)
		 (pos g))))
