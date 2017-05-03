;; EVENT DEFINITIONS ...
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

(define-event
  :long-name midi-event
  :short-name mid
  :parent-events (tuned-instrument-event)  
  :direct-parameters (pitch))

;; ready for ambisonics
;; pos is the simple stereo position,
;; azimuth, elevation and distance the ambisonics parameters
(define-event
  :long-name spatial-event
  :short-name pos
  :parent-events (event)
  :parameters ((pos event-position 0.0)
	       (azi event-azimuth 0.0)
	       (ele event-elevation 0.0)
	       (dist event-distance 0.0)) 
  :direct-parameters (pos))

;; additional constructor for convenience reasons ...
;; tbd - alias macro
(defun ambi-pos (azi ele &key (dist 0) (tags nil) (combi-fun #'replace-value))
  (make-instance 'spatial-event :pos 0.0 :azi azi :ele ele
		 :dist dist :tags tags :ambi-p t :combi-fun combi-fun))

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
  :parameters ((pf-freq event-fp-freq 1000)
	       (pf-q event-pf-q 10)
	       (pf-gain event-pf-gain 0.0)) 
  :direct-parameters (pf-freq))

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
	       (sample-location event-sample-location)) 
  :direct-parameters (sample-folder sample-file))

;; additional method after grain event initialization ...
(defmethod initialize-instance :after ((g grain-event) &key)
  (setf (event-sample-location g)
	(concatenate 'string *sample-root*
		     (event-sample-folder g) "/" (event-sample-file g) ".wav")))

(define-event
  :long-name frequency-range-event
  :short-name freq-range
  :parent-events (event)
  :parameters ((freq-min event-freq-min 410)
	       (freq-max event-freq-max 420)) 
  :direct-parameters (freq-min freq-max))

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
  :direct-parameters (freq-min freq-max))

(define-event
  :long-name control-event
  :short-name ctrl
  :parent-events (event)
  :parameters ((control-function event-control-function)) 
  :direct-parameters (control-function))

;; the transition between events is just a different type of event,
;; if you ask me ... 
(define-event
  :long-name transition
  :short-name transition
  :parent-events (event)
  :parameters ((dur transition-duration)) 
  :direct-parameters (dur))




				        


