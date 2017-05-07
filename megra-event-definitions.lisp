;; EVENT DEFINITIONS ...
;; the root event is defined in megra-event-base ...
(in-package :megra)
;; accumulator class ...
(define-event
  :long-name incomplete-event
  :short-name incomplete
  :parent-events (event))

;; those "abstract" events provide the building blocks
;; for the events that will later on produce a sound 
(define-event
  :long-name pitch-event
  :short-name pitch
  :parent-events (event)
  :parameters ((pitch event-pitch 43)) 
  :direct-parameters (pitch))

(define-event
  :long-name message-event
  :short-name message
  :parent-events (event)
  :parameters ((msg event-message)) 
  :direct-parameters (msg))

(define-event
  :long-name duration-event
  :short-name dur
  :parent-events (event)
  :parameters ((dur event-duration 512)) 
  :direct-parameters (dur))

(define-event
  :long-name level-event
  :short-name lvl
  :parent-events (event)
  :parameters ((lvl event-level 0.3)) 
  :direct-parameters (lvl))

(define-event
  :long-name instrument-event
  :short-name inst
  :parent-events (event)
  :parameters ((inst event-instrument)) 
  :direct-parameters (inst))

(define-event
  :long-name tuned-instrument-event
  :short-name tuned-instrument-event
  :parent-events (pitch-event level-event instrument-event duration-event))
(in-package :megra)

;;;;;;;;;;;;;;;;;;;;;;;;
;; Generic MIDI event ;;
;;;;;;;;;;;;;;;;;;;;;;;;
(define-event
  :long-name midi-event
  :short-name mid
  :parent-events (tuned-instrument-event)  
  :direct-parameters (pitch)
  :handler (events (cm::new cm::midi
		   :time *global-midi-delay*
		   :keynum (event-pitch evt)
		   :duration (coerce (* (event-duration evt) 0.001) 'single-float)
		   :amplitude (round (* 127 (event-level evt))))
		   :at (incudine:now)))
;; end midi event ...

;; megra is ready for ambisonics !
;; pos is the simple stereo position,
;; azimuth, elevation and distance the ambisonics parameters
(define-event
  :long-name spatial-event
  :short-name pos
  :parent-events (event)
  :parameters ((pos event-position 0.5)
	       (azi event-azimuth 0.0)
	       (ele event-elevation 0.0)
	       (dist event-distance 0.0)) 
  :direct-parameters (pos))

;; additional constructor for convenience reasons ...
;; tbd - alias macro
(defun ambi-pos (azi ele &key (dist 0) (tags nil) (combi-fun #'replace-value))
  (make-instance 'spatial-event :pos 0.0 :azi azi :ele ele
		 :dist dist :tags tags :ambi-p t :combi-fun combi-fun))

;; custom print function to take into accout the two constructors
(defmethod print-event ((s spatial-event) &key)
  (if (event-ambi-p s)
      (format nil "(ambi-pos ~a ~a :dist ~a~a~a)"	  	  
	      (event-azimuth s)
	      (event-elevation s)
	      (event-distance s)
	      (print-tags (event-tags s))
	      (print-combi-fun (value-combine-function s)))
      (format nil "(pos ~a ~a~a)"	  	  
	      (event-position s)
	      (print-tags (event-tags s))
	      (print-combi-fun (value-combine-function s)))))

(define-event
  :long-name rate-event
  :short-name rate
  :parent-events (event)
  :parameters ((rate event-rate 1.0)) 
  :direct-parameters (rate))

(define-event
  :long-name attack-event
  :short-name atk
  :parent-events (event)
  :parameters ((atk event-attack 5)) 
  :direct-parameters (atk))

(define-event
  :long-name release-event
  :short-name rel
  :parent-events (event)
  :parameters ((rel event-release 5)) 
  :direct-parameters (rel))

(define-event
  :long-name start-event
  :short-name start
  :parent-events (event)
  :parameters ((start event-start 0.0)) 
  :direct-parameters (start))

(define-event
  :long-name reverb-event
  :short-name rev
  :parent-events (event)
  :parameters ((rev event-reverb)) 
  :direct-parameters (rev))

(define-event
  :long-name filter-hp-event
  :short-name filter-hp
  :parent-events (event)
  :parameters ((hp-freq event-hp-freq 10) (hp-q event-hp-q 0.4)) 
  :direct-parameters (hp-freq))

(define-event
  :long-name filter-lp-event
  :short-name filter-lp
  :parent-events (event)
  :parameters ((lp-freq event-lp-freq 19000)
	       (lp-q event-lp-q 0.4)
	       (lp-dist event-lp-dist 0.0)) 
  :direct-parameters (lp-freq))

(define-event
  :long-name filter-peak-event
  :short-name filter-peak
  :parent-events (event)
  :parameters ((pf-freq event-pf-freq 1000)
	       (pf-q event-pf-q 10)
	       (pf-gain event-pf-gain 0.0)) 
  :direct-parameters (pf-freq))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GRAIN EVENT (slightly longer);;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-event
  :long-name grain-event
  :short-name grain
  :parent-events (level-event
		  duration-event
		  spatial-event
		  start-event
		  rate-event
		  attack-event
		  release-event
		  filter-hp-event
		  filter-lp-event
	          filter-peak-event
		  reverb-event)
  :parameters ((sample-folder event-sample-folder)
	       (sample-file event-sample-file)
	       (sample-location event-sample-location)
	       (ambi event-ambi-p nil)) 
  :direct-parameters (sample-folder sample-file)
  :handler (handle-grain-event-incu evt) ;; currently only using include, anyway ...
  ;;(if (member 'inc (event-backends g)) )
  ;;(if (member 'sc (event-backends g)) (handle-grain-event-sc g))
  )

;; additional method after grain event initialization ...
(defmethod initialize-instance :after ((g grain-event) &key)
  (setf (event-sample-location g)
	(concatenate 'string *sample-root*
		     (event-sample-folder g) "/" (event-sample-file g) ".wav")))

;; handler method for grain event, supercollider
(defmethod handle-grain-event-sc ((g grain-event) &key)
  (unless (gethash (event-sample-location g) *buffer-directory*)
    (register-sample (event-sample-location g)))
  (let ((bufnum (gethash (event-sample-location g) *buffer-directory*)))
    (cm::send-osc  
     "/s_new"	    
     "siiisisfsfsfsfsfsfsfsfsfsfsfsfsfsfsf"
     "grain_2ch" -1 0 1
     "bufnum" bufnum
     "lvl" (coerce (event-level g) 'float)
     "rate" (coerce (event-rate g) 'float)
     "start" (coerce (event-start g) 'float)
     "lp_freq" (coerce (event-lp-freq g) 'float)
     "lp_q" (coerce (event-lp-q g) 'float)
     "lp_dist" (coerce (event-lp-dist g) 'float)
     "pf_freq" (coerce (event-pf-freq g) 'float)
     "pf_q" (coerce (event-pf-q g) 'float)
     "pf_gain" (coerce (event-pf-gain g) 'float)
     "hp_freq" (coerce (event-hp-freq g) 'float)
     "hp_q" (coerce (event-hp-q g)  'float)
     "a" (coerce (* (event-attack g) 0.001) 'float)
     "length" (coerce (* (- (event-duration g) (event-attack g) (event-release g)) 0.001) 'float)
     "r" (coerce (* (event-release g) 0.001) 'float)
     "pos" (coerce (- (event-position g) 0.5) 'float))))

;; handler method for grain event, incudine
(defmethod handle-grain-event-incu ((g grain-event) &key)
  (unless (gethash (event-sample-location g) *buffer-directory*)
    (let* ((buffer (incudine:buffer-load (event-sample-location g)))
	   (bdata (make-buffer-data :buffer buffer
				    :buffer-rate (/ (incudine:buffer-sample-rate buffer)
						    (incudine:buffer-frames buffer))
				    :buffer-frames (incudine:buffer-frames buffer))))
      (setf (gethash (event-sample-location g) *buffer-directory*) bdata)))
  (let ((bdata (gethash (event-sample-location g) *buffer-directory*)))    
    (cond ((not (event-ambi-p g))
	   (scratch::megra-grain-rev (buffer-data-buffer bdata)
		 (buffer-data-buffer-rate bdata)
		 (buffer-data-buffer-frames bdata)
		 (event-level g)
		 (event-rate g)
		 (event-start g)
		 (event-lp-freq g)
		 (event-lp-q g)
		 (event-lp-dist g)
		 (event-pf-freq g)
		 (event-pf-q g)
		 (event-pf-gain g)
		 (event-hp-freq g)
		 (event-hp-q g)
		 (* (event-attack g) 0.001)
		 (* (- (event-duration g) (event-attack g) (event-release g)) 0.001)
		 (* (event-release g) 0.001)
		 (event-position g)
		 (event-reverb g)
		 scratch::*rev-chapel*))
	((event-ambi-p g)  
	 (scratch::megra-grain-ambi-rev (buffer-data-buffer bdata)
		 (buffer-data-buffer-rate bdata)
		 (buffer-data-buffer-frames bdata)
		 (event-level g)
		 (event-rate g)
		 (event-start g)
		 (event-lp-freq g)
		 (event-lp-q g)
		 (event-lp-dist g)
		 (event-pf-freq g)
		 (event-pf-q g)
		 (event-pf-gain g)
		 (event-hp-freq g)
		 (event-hp-q g)
		 (* (event-attack g) 0.001)
		 (* (- (event-duration g) (event-attack g) (event-release g)) 0.001)
		 (* (event-release g) 0.001)
		 (+ (event-azimuth g) *global-azimuth-offset*)
		 (+ (event-elevation g) *global-elevation-offset*)
		 (event-reverb g)
		 scratch::*rev-chapel*)))))
;; end grain-event ...

(define-event
  :long-name frequency-range-event
  :short-name freq-range
  :parent-events (event)
  :parameters ((freq-min event-freq-min 410)
	       (freq-max event-freq-max 420)) 
  :direct-parameters (freq-min freq-max))

;; gendy-based event ... extremly cpu-intensive ... 
(define-event
  :long-name gendy-event
  :short-name gendy
  :parent-events (level-event
		  duration-event
		  filter-lp-event
		  frequency-range-event
		  attack-event
		  release-event
		  reverb-event
		  spatial-event)
  :parameters ((adstr event-amp-distr 1)
	       (ddstr event-dur-distr 1)
	       (adstr-par  event-amp-distr-param 1)
	       (ddstr-par  event-dur-distr-param 1)	       
	       (a-scl  event-amp-scale 0.01)
	       (d-scl  event-dur-scale 0.01)) 
  :direct-parameters (freq-min freq-max)
  :handler
  (cond ((not (event-ambi-p evt))
	 (scratch::gendy-stereo-rev
	  (event-amp-distr evt)
	  (event-dur-distr evt)
	  (event-amp-distr-param evt)
	  (event-dur-distr-param evt)
	  (event-freq-min evt)
	  (event-freq-max evt)
	  (event-amp-scale evt)
	  (event-dur-scale evt)
	  (event-level evt)
	  (event-lp-freq evt)
	  (event-lp-q evt)
	  (event-lp-dist evt)
	  (* (event-attack evt) 0.001)
	  (* (- (event-duration evt) (event-attack evt) (event-release evt)) 0.001)
	  (* (event-release evt) 0.001)
	  (event-position evt)
	  (event-reverb evt)
	  scratch::*rev-chapel*))
	((event-ambi-p evt)
	 (scratch::gendy-stereo-rev
	  (event-amp-distr evt)
	  (event-dur-distr evt)
	  (event-amp-distr-param evt)
	  (event-dur-distr-param evt)
	  (event-freq-min evt)
	  (event-freq-max evt)
	  (event-amp-scale evt)
	  (event-dur-scale evt)
	  (event-level evt)
	  (event-lp-freq evt)
	  (event-lp-q evt)
	  (event-lp-dist evt)
	  (* (event-attack evt) 0.001)
	  (* (- (event-duration evt) (event-attack evt) (event-release evt)) 0.001)
	  (* (event-release evt) 0.001)
	  (event-azimuth evt)
	  (event-elevation evt)
	  (event-reverb evt)
	  scratch::*rev-chapel*))))

(define-event
  :long-name control-event
  :short-name ctrl
  :parent-events (event)
  :parameters ((control-function event-control-function)) 
  :direct-parameters (control-function)
  ;; just call the specified control function ... 
  :handler (funcall (control-function evt)))

;; the transition between events is just a different type of event,
;; if you ask me ... 
(define-event
  :long-name transition
  :short-name transition
  :parent-events (event)
  :parameters ((dur transition-duration)) 
  :direct-parameters (dur))




				        


