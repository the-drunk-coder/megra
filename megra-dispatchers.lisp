;; event dispatchers and related stuff ... 

(defclass dispatcher ()
  ((perform-dispatch)
   (handle-events)
   (handle-transition)))

;; simple time-recursive dispatching
(defmethod perform-dispatch ((d dispatcher) proc time &key)
  (when (and (gethash proc *processor-directory*) (is-active (gethash proc *processor-directory*)) )
    (scratch::rt-eval () (handle-events d (pull-events (gethash proc *processor-directory*)))
      (let* ((trans-time (handle-transition d (car
					       (pull-transition
						(gethash proc *processor-directory*)))))
	     (next (+ time #[trans-time ms])))
	(incudine:at next #'perform-dispatch d proc next)))))

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
	    (princ (event-message event))
	    (princ " from ")
	    (princ (event-source event))
	    (princ ", ")) events))

;; the main event dispatcher
(defclass event-dispatcher (dispatcher) ())

(defmethod handle-events ((e event-dispatcher) events &key)
  (mapc #'handle-event events))

;; see what we can still do with this ... 
(defmethod handle-event ((e incomplete-event) &key))

;; if nothing else helps ...
(defmethod handle-event ((e event) &key))

;; handler methods for individual events ... 
(defmethod handle-event ((m midi-event) &key)
  (events (cm::new cm::midi
	       :time 0
	       :keynum (event-pitch m)
	       :duration (coerce (* (event-duration m) 0.001) 'single-float)
	       :amplitude (round (* 127 (event-level m))))
	  :at (incudine:now)))

(defmethod handle-event ((c control-event) &key)
  (funcall (control-function c)))

(defmethod handle-event ((g grain-event) &key)
  (if (member 'inc (event-backends g)) (handle-grain-event-incu g))
  (if (member 'sc (event-backends g)) (handle-grain-event-sc g)))

(defmethod handle-grain-event-sc ((g grain-event) &key)
  (unless (gethash (sample-location g) *buffer-directory*)
    (register-sample (sample-location g)))
  (let ((bufnum (gethash (sample-location g) *buffer-directory*)))
    (output (new cm::osc 
	    :path "/s_new"
	    :time (now)
	    :types "siiisisfsfsfsfsfsfsfsfsfsfsfsfsfsfsf"
	    :message `("grain_2ch" -1 0 1
		       "bufnum" ,bufnum
		       "lvl" ,(event-level g)
		       "rate" ,(rate g)
		       "start" ,(start g)
		       "lp_freq" ,(lp-freq g)
		       "lp_q" ,(lp-q g)
		       "lp_dist" ,(lp-dist g)
		       "pf_freq" ,(pf-freq g)
		       "pf_q" ,(pf-q g)
		       "pf_gain" ,(pf-gain g)
		       "hp_freq" ,(hp-freq g)
		       "hp_q" ,(hp-q g)
		       "a" ,(* (atk g) 0.001)
		       "length" ,(* (- (event-duration g) (atk g) (rel g)) 0.001)
		       "r" ,(* (rel g) 0.001)
		       "pos" ,(event-position g))))))

(defmethod handle-grain-event-incu ((g grain-event) &key)
  (unless (gethash (sample-location g) *buffer-directory*)
    (let* ((buffer (incudine:buffer-load (sample-location g)))
	   (bdata (make-buffer-data :buffer buffer
				    :buffer-rate (/ (incudine:buffer-sample-rate buffer)
						    (incudine:buffer-frames buffer))
				    :buffer-frames (incudine:buffer-frames buffer))))
      (setf (gethash (sample-location g) *buffer-directory*) bdata)))
  (let ((bdata (gethash (sample-location g) *buffer-directory*)))
    (if (> (rev g) 0.0)
	(scratch::megra-grain-rev (buffer-data-buffer bdata)
		 (buffer-data-buffer-rate bdata)
		 (buffer-data-buffer-frames bdata)
		 (event-level g)
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
		 (* (- (event-duration g) (atk g) (rel g)) 0.001)
		 (* (rel g) 0.001)
		 (event-position g)
		 (rev g)
		 scratch::*rev-chapel*)
	(scratch::megra-grain (buffer-data-buffer bdata)
		 (buffer-data-buffer-rate bdata)
		 (buffer-data-buffer-frames bdata)
		 (event-level g)
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
		 (* (- (event-duration g) (atk g) (rel g)) 0.001)
		 (* (rel g) 0.001)
		 (event-position g)
		 scratch::*rev-chapel*
		 ))))
