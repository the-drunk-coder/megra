(in-package :megra)

;; EVENT DEFINITIONS ...
;; the root event is defined in megra-event-base ...

;; accumulator class ...
(define-event
  :long-name incomplete-event
  :short-name incomplete
  :parent-events (event))


(define-event
  :long-name silent-event
  :short-name silence
  :parent-events (event))

;; those "abstract" events provide the building blocks
;; for the events that will later on produce a sound

;; parameter format:
;; (name accessor-name default minimum maximum)
;; name and accessor name are mandatory, the rest optional ... 
(define-event
  :long-name pitch-event
  :short-name pitch
  :parent-events (event)
  :parameters ((pitch event-pitch 43 1 19000)) 
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
  :parameters ((dur event-duration 512 10)) ;; minimum duration, but no max ...   
  :direct-parameters (dur))

(define-event
  :long-name level-event
  :short-name lvl
  :parent-events (event)
  :parameters ((lvl event-level 0.3 0.0 0.9)) 
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

;;;;;;;;;;;;;;;;;
;; MIDI events ;;
;;;;;;;;;;;;;;;;;
(define-event
  :long-name midi-event
  :short-name mid
  :abstract-event nil
  :parent-events (tuned-instrument-event)  
  :parameters ((channel event-channel 0))
  :direct-parameters (pitch)
  :handler (events (cm::new cm::midi
		     :time *global-midi-delay*			  
		     :channel (event-channel evt)
		     :keynum (event-pitch evt)
		     :duration (coerce
				(* (event-duration evt) 0.001) 'single-float)
		     :amplitude (round (* 127 (event-level evt))))))

(define-event-alias
  :long-name midi-event
  :alias gb0
  :direct-parameters (pitch)
  :alias-defaults ((channel 0)))

(define-event-alias
  :long-name midi-event
  :alias gb1
  :direct-parameters (pitch)
  :alias-defaults ((channel 1)))

(define-event-alias
  :long-name midi-event
  :alias gb2
  :direct-parameters (pitch)
  :alias-defaults ((channel 2)))

(define-event-alias
  :long-name midi-event
  :alias gb3
  :direct-parameters (pitch)
  :alias-defaults ((channel 3)))

(define-event-alias
  :long-name midi-event
  :alias volca
  :direct-parameters (pitch)
  :alias-defaults ((channel 5)))

(define-event
  :long-name midi-control-change-event
  :short-name mid-cc
  :parent-events (event)  
  :parameters ((channel event-cc-channel 0)
	       (controller event-cc-controller 0)
	       (value event-cc-value 0))
  :direct-parameters (controller value)
  :handler (events (cm::new cm::midi-control-change
		     :time *global-midi-delay*
		     :channel (event-cc-channel evt)
		     :controller (event-cc-controller evt)
		     :value (car (multiple-value-list
				  (round (event-cc-value evt)))))	     
		   :at (incudine:now)))

(define-event-alias
  :long-name midi-control-change-event
  :alias gb2-shape
  :direct-parameters (value)
  :alias-defaults ((channel 2)
		   (controller 1)))

(define-event-alias
  :long-name midi-control-change-event
  :alias gb2-offset
  :direct-parameters (value)
  :alias-defaults ((channel 2)
		   (controller 2)))

(define-event-alias
  :long-name midi-control-change-event
  :alias gb2-sweep
  :direct-parameters (value)
  :alias-defaults ((channel 2)
		   (controller 3)))
;; end midi events ...

;; megra is ready for ambisonics !
;; pos is the simple stereo position,
;; azimuth, elevation and distance the ambisonics parameters
(define-event
  :long-name pan-event
  :short-name pos
  :parent-events (event)
  :parameters ((pos event-position 0.5 -1.5 1.5)) 
  :direct-parameters (pos))

(define-event
  :long-name ambi-event
  :short-name ambi
  :parent-events (event)
  :parameters ((azi event-azimuth 0.0)
	       (ele event-elevation 0.0)
	       (dst event-distance 2.0)) 
  :direct-parameters (azi ele))

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
  :parameters ((atk event-attack 5 1)) 
  :direct-parameters (atk))

(define-event
  :long-name decay-event
  :short-name dec
  :parent-events (event)
  :parameters ((dec event-decay 20 1)) 
  :direct-parameters (dec))

(define-event
  :long-name sustain-event
  :short-name sus
  :parent-events (event)
  :parameters ((sus event-sustain 50 1)) 
  :direct-parameters (sus))

(define-event
  :long-name release-event
  :short-name rel
  :parent-events (event)
  :parameters ((rel event-release 5 1)) 
  :direct-parameters (rel))

(define-event
  :long-name pulsewidth-event
  :short-name pw
  :parent-events (event)
  :parameters ((pw event-pulsewidth 0.5 0.01 0.09)) 
  :direct-parameters (pw))

(define-event
  :long-name level-lfo-event
  :short-name lvl-lfo
  :parent-events (event)
  :parameters ((lvl-lfo-speed event-level-lfo-speed 0.0 0.0 20000.0)
	       (lvl-lfo-depth event-level-lfo-depth 0.0 0.0 1.0)
	       (lvl-lfo-phase event-level-lfo-phase 0.0 0.0 1.0)) 
  :direct-parameters (lvl-lfo-speed))

(define-event
  :long-name frequency-lfo-event
  :short-name freq-lfo
  :parent-events (event)
  :parameters ((freq-lfo-speed event-freq-lfo-speed 0.0 0.0 20000.0)
	       (freq-lfo-depth event-freq-lfo-depth 0.0 0.0 1.0)
	       (freq-lfo-phase event-freq-lfo-phase 0.0 0.0 1.0)) 
  :direct-parameters (freq-lfo-speed))

(define-event
  :long-name lowpass-frequency-lfo-event
  :short-name lp-freq-lfo
  :parent-events (event)
  :parameters ((lp-freq-lfo-speed event-lp-freq-lfo-speed 0.0 0.0 20000.0)
	       (lp-freq-lfo-depth event-lp-freq-lfo-depth 0.0 0.0 1.0)
	       (lp-freq-lfo-phase event-lp-freq-lfo-phase 0.0 0.0 1.0)) 
  :direct-parameters (lp-freq-fo-speed))

(define-event
  :long-name highpass-frequency-lfo-event
  :short-name hp-freq-lfo
  :parent-events (event)
  :parameters ((hp-lfo-speed event-hp-lfo-speed 0.0 0.0 20000.0)
	       (hp-lfo-depth event-hp-lfo-depth 0.0 0.0 1.0)
	       (hp-lfo-phase event-hp-lfo-phase 0.0 0.0 1.0)) 
  :direct-parameters (hp-lfo-speed))

(define-event
  :long-name peak-frequency-lfo-event
  :short-name peak-freq-lfo
  :parent-events (event)
  :parameters ((peak-lfo-speed event-peak-lfo-speed 0.0 0.0 20000.0)
	       (peak-lfo-depth event-peak-lfo-depth 0.0 0.0 1.0)
	       (peak-lfo-phase event-peak-lfo-phase 0.0 0.0 1.0)) 
  :direct-parameters (peak-lfo-speed))

(define-event
  :long-name peak-gain-lfo-event
  :short-name peak-gain-lfo
  :parent-events (event)
  :parameters ((peak-gain-lfo-speed event-peak-gain-lfo-speed 0.0 0.0 20000.0)
	       (peak-gain-lfo-depth event-peak-gain-lfo-depth 0.0 0.0 1.0)
	       (peak-gain-lfo-phase event-peak-gain-lfo-phase 0.0 0.0 1.0)) 
  :direct-parameters (peak-gain-lfo-speed))

(define-event
  :long-name mix-event
  :short-name mix
  :parent-events (event)
  :parameters ((mix event-mix 0.2 0.0 1.0)) 
  :direct-parameters (mix))

(define-event
  :long-name velocity-event
  :short-name vel
  :parent-events (event)
  :parameters ((vel event-velocity 0.8 0.0 1.0)) 
  :direct-parameters (vel))

(define-event
  :long-name mod-index-event
  :short-name mod-idx
  :parent-events (event)
  :parameters ((mod-idx event-mod-index 0.3 0.0 0.6)) 
  :direct-parameters (mod-idx))

(define-event
  :long-name start-event
  :short-name start
  :parent-events (event)
  :parameters ((start event-start 0.0 0.0 1.0)) 
  :direct-parameters (start))

(define-event
  :long-name reverb-event
  :short-name rev
  :parent-events (event)
  :parameters ((rev event-reverb 0.0 0.0 0.4)) 
  :direct-parameters (rev))

(define-event
  :long-name filter-hp-event
  :short-name filter-hp
  :parent-events (event)
  :parameters ((hp-freq event-hp-freq 10 5 19000)
	       (hp-q event-hp-q 0.4 0.09 0.9)) 
  :direct-parameters (hp-freq))

(define-event
  :long-name filter-lp-event
  :short-name filter-lp
  :parent-events (event)
  :parameters ((lp-freq event-lp-freq 19000 10 19000)
	       (lp-q event-lp-q 0.1 0.07 0.9)
	       (lp-dist event-lp-dist 0.0 0.0 1.0)) 
  :direct-parameters (lp-freq))

(define-event
  :long-name filter-peak-event
  :short-name filter-peak
  :parent-events (event)
  :parameters ((pf-freq event-pf-freq 1000 10 19000)
	       (pf-q event-pf-q 10 1 40)
	       (pf-gain event-pf-gain 0.0 -10.0 10.0)) 
  :direct-parameters (pf-freq))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GRAIN EVENT (slightly longer);;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-event
  :long-name grain-event
  :short-name grain
  :abstract-event nil
  :parent-events (level-event
		  duration-event
		  pan-event
		  start-event
		  rate-event
		  attack-event
		  release-event
		  filter-hp-event
		  filter-lp-event
		  lowpass-frequency-lfo-event
	          filter-peak-event
		  reverb-event)
  :parameters ((sample-folder grain-sample-folder)
	       (sample-file grain-sample-file)
	       (sample-location grain-sample-location)) 
  :direct-parameters (sample-folder sample-file)
  :handler (progn
	     (if (member 'inc (event-backends evt)) (handle-grain-event-incu evt))
	     (if (member 'sc (event-backends evt)) (handle-grain-event-sc evt timestamp))))

(define-event
  :long-name grain-event-ambi
  :short-name grain-ambi
  :abstract-event nil
  :parent-events (level-event
		  duration-event
		  ambi-event
		  start-event
		  rate-event
		  attack-event
		  release-event
		  filter-hp-event
		  filter-lp-event
		  lowpass-frequency-lfo-event
	          filter-peak-event
		  reverb-event)
  :parameters ((sample-folder grain-ambi-sample-folder)
	       (sample-file grain-ambi-sample-file)
	       (sample-location grain-ambi-sample-location)) 
  :direct-parameters (sample-folder sample-file)
  :handler (progn
	     (if (member 'inc (event-backends evt)) (handle-grain-event-incu evt))
	     (if (member 'sc (event-backends evt)) (handle-grain-event-sc-ambi evt timestamp))))


(define-event
  :long-name grain-event-4ch
  :short-name grain-4ch
  :abstract-event nil
  :parent-events (level-event
		  duration-event
		  pan-event
		  start-event
		  rate-event
		  attack-event
		  release-event
		  filter-hp-event
		  filter-lp-event
		  lowpass-frequency-lfo-event
	          filter-peak-event
		  reverb-event)
  :parameters ((sample-folder grain-4ch-sample-folder)
	       (sample-file grain-4ch-sample-file)
	       (sample-location grain-4ch-sample-location)) 
  :direct-parameters (sample-folder sample-file)
  :handler (handle-grain-event-sc-4ch evt timestamp))


(define-event
  :long-name grain-event-8ch
  :short-name grain-8ch
  :abstract-event nil
  :parent-events (level-event
		  duration-event
		  pan-event
		  start-event
		  rate-event
		  attack-event
		  release-event
		  filter-hp-event
		  filter-lp-event
		  lowpass-frequency-lfo-event
	          filter-peak-event
		  reverb-event)
  :parameters ((sample-folder grain-8ch-sample-folder)
	       (sample-file grain-8ch-sample-file)
	       (sample-location grain-8ch-sample-location)) 
  :direct-parameters (sample-folder sample-file)
  :handler (handle-grain-event-sc-8ch evt timestamp))

(define-event
  :long-name grain-event-nores
  :short-name nores
  :abstract-event nil
  :parent-events (level-event
		  duration-event
		  pan-event
		  start-event
		  rate-event
		  attack-event
		  release-event
		  filter-hp-event
		  filter-lp-event
		  lowpass-frequency-lfo-event
	          filter-peak-event
		  reverb-event)
  :parameters ((sample-folder nores-sample-folder)
	       (sample-file nores-sample-file)
	       (sample-location nores-sample-location)) 
  :direct-parameters (sample-folder sample-file)
  :handler (progn
	     (if (member 'inc (event-backends evt)) (handle-grain-event-incu-nores evt))
	     (if (member 'sc (event-backends evt)) (handle-grain-event-sc-nores evt timestamp))))

(define-event
  :long-name grain-event-nores-ambi
  :short-name nores-ambi
  :abstract-event nil
  :parent-events (level-event
		  duration-event
		  ambi-event
		  start-event
		  rate-event
		  attack-event
		  release-event
		  filter-hp-event
		  filter-lp-event
		  lowpass-frequency-lfo-event
	          filter-peak-event
		  reverb-event)
  :parameters ((sample-folder nores-ambi-sample-folder)
	       (sample-file nores-ambi-sample-file)
	       (sample-location nores-ambi-sample-location)) 
  :direct-parameters (sample-folder sample-file)
  :handler (if (member 'sc (event-backends evt)) (handle-grain-event-sc-nores-ambi evt timestamp)))

(define-event
  :long-name grain-event-24db
  :short-name grain-24db
  :abstract-event nil
  :parent-events (level-event
		  duration-event
		  pan-event
		  start-event
		  rate-event
		  attack-event
		  release-event
		  filter-hp-event
		  filter-lp-event
		  lowpass-frequency-lfo-event
	          filter-peak-event
		  reverb-event)
  :parameters ((sample-folder twofourdb-sample-folder)
	       (sample-file twofourdb-sample-file)
	       (sample-location twofourdb-sample-location)) 
  :direct-parameters (sample-folder sample-file)
  :handler (progn
	     (if (member 'inc (event-backends evt)) (handle-grain-event-incu-24db evt))
	     (if (member 'sc (event-backends evt)) (handle-grain-event-sc-24db evt timestamp))))

(define-event
  :long-name grain-event-24db-ambi
  :short-name grain-24db-ambi
  :abstract-event nil
  :parent-events (level-event
		  duration-event
		  ambi-event
		  start-event
		  rate-event
		  attack-event
		  release-event
		  filter-hp-event
		  filter-lp-event
		  lowpass-frequency-lfo-event
	          filter-peak-event
		  reverb-event)
  :parameters ((sample-folder twofourdb-ambi-sample-folder)
	       (sample-file twofourdb-ambi-sample-file)
	       (sample-location twofourdb-ambi-sample-location)) 
  :direct-parameters (sample-folder sample-file)
  :handler (if (member 'sc (event-backends evt)) (handle-grain-event-sc-24db-ambi evt timestamp)))


;; additional method after grain event initialization ...
(defmethod initialize-instance :after ((g grain-event) &key)
  (setf (grain-sample-location g)
	(concatenate 'string *sample-root*
		     (grain-sample-folder g) "/" (grain-sample-file g) "." cm::*sample-type* )))

;; additional method after grain event initialization ...
(defmethod initialize-instance :after ((g grain-event-4ch) &key)
  (setf (grain-4ch-sample-location g)
	(concatenate 'string *sample-root*
		     (grain-4ch-sample-folder g) "/" (grain-4ch-sample-file g) "." cm::*sample-type* )))

(defmethod initialize-instance :after ((g grain-event-ambi) &key)
  (setf (grain-ambi-sample-location g)
	(concatenate 'string *sample-root*
		     (grain-ambi-sample-folder g) "/" (grain-ambi-sample-file g) "." cm::*sample-type* )))

;; additional method after grain event initialization ...
(defmethod initialize-instance :after ((g grain-event-8ch) &key)
  (setf (grain-8ch-sample-location g)
	(concatenate 'string *sample-root*
		     (grain-8ch-sample-folder g) "/" (grain-8ch-sample-file g) "." cm::*sample-type* )))

(defmethod initialize-instance :after ((g grain-event-nores) &key)
  (setf (nores-sample-location g)
	(concatenate 'string *sample-root*
		     (nores-sample-folder g) "/" (nores-sample-file g) "." cm::*sample-type*)))

(defmethod initialize-instance :after ((g grain-event-nores-ambi) &key)
  (setf (nores-ambi-sample-location g)
	(concatenate 'string *sample-root*
		     (nores-ambi-sample-folder g) "/" (nores-ambi-sample-file g) "." cm::*sample-type*)))

(defmethod initialize-instance :after ((g grain-event-24db) &key)
  (setf (twofourdb-sample-location g)
	(concatenate 'string *sample-root*
		     (twofourdb-sample-folder g) "/" (twofourdb-sample-file g) "." cm::*sample-type*)))

(defmethod initialize-instance :after ((g grain-event-24db-ambi) &key)
  (setf (twofourdb-ambi-sample-location g)
	(concatenate 'string *sample-root*
		     (twofourdb-ambi-sample-folder g) "/" (twofourdb-ambi-sample-file g) "." cm::*sample-type*)))

;; end grain-event ...

(define-event
  :long-name harm-event
  :short-name harm
  :parent-events (event)
  :parameters ((harm event-harmonies 3 1 5)) 
  :direct-parameters (harm))

(define-event
  :long-name buzz-event
  :short-name buzz
  :abstract-event nil
  :parent-events (level-event
		  harm-event
		  duration-event
		  pan-event
		  pitch-event
		  attack-event
		  release-event		  
		  filter-lp-event	          
		  reverb-event)
  :direct-parameters (pitch)
  :handler (progn
	     (if (member 'inc (event-backends evt)) (handle-buzz-event-incu evt))
	     (if (member 'sc (event-backends evt))
		 (handle-buzz-event-sc evt timestamp))))

(define-event
  :long-name buzz-event-8ch
  :short-name buzz-8ch
  :abstract-event nil
  :parent-events (level-event
		  harm-event
		  duration-event
		  pan-event
		  pitch-event
		  attack-event
		  release-event		  
		  filter-lp-event	          
		  reverb-event)
  :direct-parameters (pitch)
  :handler (progn	     
	     (if (member 'sc (event-backends evt))
		 (handle-buzz-event-sc-8ch evt timestamp))))


(define-event
  :long-name square-adsr-event
  :short-name sqr-adsr
  :abstract-event nil
  :parent-events (level-event
		  pulsewidth-event		  
		  pan-event
		  pitch-event
		  attack-event
		  decay-event
		  sustain-event
		  release-event		  
		  filter-lp-event	          
		  reverb-event)
  :direct-parameters (pitch)
  :handler (progn	     
	     (if (member 'sc (event-backends evt))
		 (handle-square-event-sc evt timestamp))))

(define-event
  :long-name saw-adsr-event
  :short-name saw-adsr
  :parent-events (level-event		 
		  pan-event
		  pitch-event
		  attack-event
		  decay-event
		  sustain-event
		  release-event		  
		  filter-lp-event	          
		  reverb-event)
  :direct-parameters (pitch)
  :handler (progn	     
	     (if (member 'sc (event-backends evt))
		 (handle-saw-adsr-event-sc evt timestamp))))

(define-event
  :long-name saw-event
  :short-name saw
  :parent-events (level-event		 
		  duration-event
		  pan-event
		  pitch-event
		  attack-event
		  release-event		  
		  filter-lp-event	          
		  reverb-event)
  :direct-parameters (pitch)
  :handler (progn
	     (if (member 'inc (event-backends evt)) (handle-saw-event-incu evt))
	     (if (member 'sc (event-backends evt))
		 (handle-saw-event-sc evt timestamp))))

(define-event
  :long-name saw-event-8ch
  :short-name saw-8ch
  :abstract-event nil
  :parent-events (level-event		 
		  duration-event
		  pan-event
		  pitch-event
		  attack-event
		  release-event		  
		  filter-lp-event	          
		  reverb-event)
  :direct-parameters (pitch)
  :handler (progn	     
	     (if (member 'sc (event-backends evt))
		 (handle-saw-event-sc-8ch evt timestamp))))



(define-event
  :long-name square-event
  :short-name sqr
  :abstract-event nil
  :parent-events (level-event		 
		  duration-event
		  pan-event
		  pitch-event
		  attack-event
		  release-event		  
		  filter-lp-event
		  pulsewidth-event
		  reverb-event)
  :direct-parameters (pitch)
  :handler (progn
	     (if (member 'inc (event-backends evt)) (handle-square-event-incu evt))
	     (if (member 'sc (event-backends evt))
		 (handle-square-event-sc evt timestamp))))

(define-event
  :long-name square-event-8ch
  :short-name sqr-8ch
  :abstract-event nil
  :parent-events (level-event		 
		  duration-event
		  pan-event
		  pitch-event
		  attack-event
		  release-event		  
		  filter-lp-event
		  pulsewidth-event
		  reverb-event)
  :direct-parameters (pitch)
  :handler (progn	    
	     (if (member 'sc (event-backends evt))
		 (handle-square-event-sc-8ch evt timestamp))))


(define-event
  :long-name sine-event
  :short-name sine
  :abstract-event nil
  :parent-events (level-event		 
		  duration-event
		  pan-event
		  pitch-event
		  attack-event
		  release-event		  
		  filter-lp-event	          
		  reverb-event)
  :direct-parameters (pitch)
  :handler (progn
	     (if (member 'inc (event-backends evt)) (handle-sine-event-incu evt))
	     (if (member 'sc (event-backends evt))
		 (handle-sine-event-sc evt timestamp))))

(define-event
  :long-name sine-event-8ch
  :short-name sine-8ch
  :abstract-event nil
  :parent-events (level-event		 
		  duration-event
		  pan-event
		  pitch-event
		  attack-event
		  release-event		  
		  filter-lp-event	          
		  reverb-event)
  :direct-parameters (pitch)
  :handler (progn	     
	     (if (member 'sc (event-backends evt))
		 (handle-sine-event-sc-8ch evt timestamp))))

(define-event
  :long-name triangle-event
  :short-name tri
  :abstract-event nil
  :parent-events (level-event		 
		  duration-event
		  pan-event
		  pitch-event
		  attack-event
		  release-event		  
		  filter-lp-event	          
		  reverb-event)
  :direct-parameters (pitch)
  :handler (progn
	     (if (member 'inc (event-backends evt))
		 (handle-triangle-event-incu evt))
	     (if (member 'sc (event-backends evt))
		 (handle-triangle-event-sc evt timestamp))))

(define-event
  :long-name triangle-event-8ch
  :short-name tri-8ch
  :abstract-event nil
  :parent-events (level-event		 
		  duration-event
		  pan-event
		  pitch-event
		  attack-event
		  release-event		  
		  filter-lp-event	          
		  reverb-event)
  :direct-parameters (pitch)
  :handler (progn	
	     (if (member 'sc (event-backends evt))
		 (handle-triangle-event-sc-8ch evt timestamp))))

;; LFCub
(define-event
  :long-name cub-event
  :short-name cub
  :abstract-event nil
  :parent-events (level-event		 
		  duration-event
		  pan-event
		  pitch-event
		  attack-event
		  release-event		  
		  filter-lp-event	          
		  reverb-event)
  :direct-parameters (pitch)
  :handler (progn
	     (if (member 'inc (event-backends evt))
		 (handle-cub-event-incu evt))
	     (if (member 'sc (event-backends evt))
		 (handle-cub-event-sc evt timestamp))))

(define-event
  :long-name cub-event-8ch
  :short-name cub-8ch
  :abstract-event nil
  :parent-events (level-event		 
		  duration-event
		  pan-event
		  pitch-event
		  attack-event
		  release-event		  
		  filter-lp-event	          
		  reverb-event)
  :direct-parameters (pitch)
  :handler (progn	     
	     (if (member 'sc (event-backends evt))
		 (handle-cub-event-sc-8ch evt timestamp))))
;; LFPar
(define-event
  :long-name par-event
  :short-name par
  :parent-events (level-event		 
		  duration-event
		  pan-event
		  pitch-event
		  attack-event
		  release-event		  
		  filter-lp-event	          
		  reverb-event)
  :direct-parameters (pitch)
  :handler (progn
	     (if (member 'inc (event-backends evt))
		 (handle-par-event-incu evt))
	     (if (member 'sc (event-backends evt))
		 (handle-par-event-sc evt timestamp))))

(define-event
  :long-name par-event-8ch
  :short-name par-8ch
  :abstract-event nil
  :parent-events (level-event		 
		  duration-event
		  pan-event
		  pitch-event
		  attack-event
		  release-event		  
		  filter-lp-event	          
		  reverb-event)
  :direct-parameters (pitch)
  :handler (progn
	     (if (member 'sc (event-backends evt))
		 (handle-par-event-sc-8ch evt timestamp))))

(define-event
  :long-name meow-event
  :short-name meow
  :abstract-event nil
  :parent-events (level-event		  
		  pan-event
		  pitch-event
		  attack-event
		  decay-event
		  sustain-event
		  release-event		  
		  filter-lp-event	          
		  reverb-event)
  :direct-parameters (pitch)
  :handler (progn	     
	     (if (member 'sc (event-backends evt))
		 (handle-meow-event-sc evt timestamp))))

(define-event
  :long-name risset-event
  :short-name risset
  :abstract-event nil
  :parent-events (level-event		  
		  pan-event
		  pitch-event
		  attack-event
		  decay-event
		  sustain-event
		  release-event		  
		  filter-lp-event	          
		  reverb-event)
  :direct-parameters (pitch)
  :handler (progn	     
	     (if (member 'sc (event-backends evt))
		 (handle-risset-event-sc evt timestamp))))

(define-event
  :long-name risset-event-8ch
  :short-name risset-8ch
  :parent-events (level-event		  
		  pan-event
		  pitch-event
		  attack-event
		  decay-event
		  sustain-event
		  release-event		  
		  filter-lp-event	          
		  reverb-event)
  :direct-parameters (pitch)
  :handler (progn	     
	     (if (member 'sc (event-backends evt))
		 (handle-risset-event-sc-8ch evt timestamp))))

(define-event
  :long-name pluck-event
  :short-name pluck
  :abstract-event nil
  :parent-events (level-event		  
		  pan-event
		  duration-event
		  pitch-event		  
		  filter-lp-event	          
		  reverb-event)
  :direct-parameters (pitch)
  :handler (progn	     
	     (if (member 'sc (event-backends evt))
		 (handle-pluck-event-sc evt timestamp))))

(define-event
  :long-name pluck-event-8ch
  :short-name pluck-8ch
  :abstract-event nil
  :parent-events (level-event		  
		  pan-event
		  duration-event
		  pitch-event		  
		  filter-lp-event	          
		  reverb-event)
  :direct-parameters (pitch)
  :handler (progn	     
	     (if (member 'sc (event-backends evt))
		 (handle-pluck-event-sc-8ch evt timestamp))))


(define-event
  :long-name dx-rhodes-event
  :short-name dx-rhodes
  :abstract-event nil
  :parent-events (level-event		  
		  pan-event
		  pitch-event
		  velocity-event
		  level-lfo-event
		  mix-event
		  mod-index-event
		  reverb-event)
  :direct-parameters (pitch)
  :handler (progn	     
	     (if (member 'sc (event-backends evt))
		 (handle-dx-rhodes-event-sc evt timestamp))))

(define-event
  :long-name dx-rhodes-event-8ch
  :short-name dx-rhodes-8ch
  :abstract-event nil
  :parent-events (level-event		  
		  pan-event
		  pitch-event
		  velocity-event
		  level-lfo-event
		  mix-event
		  mod-index-event
		  reverb-event)
  :direct-parameters (pitch)
  :handler (progn	     
	     (if (member 'sc (event-backends evt))
		 (handle-dx-rhodes-event-sc-8ch evt timestamp))))


(define-event
  :long-name frequency-range-event
  :short-name freq-range
  :parent-events (event)
  :parameters ((freq-min event-freq-min 410 20 19000)
	       (freq-max event-freq-max 420 20 19000))
  :direct-parameters (freq-min freq-max))

;; gendy-based event ... extremly cpu-intensive ... 
;; not available in supercollider backend at the moment ... 
(define-event
  :long-name gendy-event
  :short-name gendy
  :abstract-event nil
  :parent-events (level-event
		  duration-event
		  filter-lp-event
		  frequency-range-event
		  attack-event
		  release-event
		  pan-event
		  reverb-event)
  :parameters ((adstr event-amp-distr 1 1 2)
	       (ddstr event-dur-distr 1 1 2 )
	       (adstr-par event-amp-distr-param 1 1 2)
	       (ddstr-par event-dur-distr-param 1 1 2)	       
	       (a-scl event-amp-scale 0.01 0.01 0.02)
	       (d-scl event-dur-scale 0.01 0.01 0.02)) 
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
  :short-name control
  :abstract-event nil
  :parent-events (event)
  :parameters ((control-function event-control-function)) 
  :direct-parameters (control-function)
  ;; don't create accessors here, as we want the control
  ;; function to be called in the handler function ... 
  :create-accessors t
  ;; just call the specified control function ... 
  :handler (incudine:nrt-funcall
	    (handler-case 	        
		(event-control-function evt)
	      (simple-error (e)
		(incudine::msg
		 error "something went wrong executing ctrl ~D" e)))))

;; shorter ... 
(defmacro ctrl (&body funs)
  `(control #'(lambda () ,@funs)))

(define-event
  :long-name population-control-event
  :short-name popctrl-ev
  :abstract-event nil
  :parent-events (event)
  :parameters ((graph-id event-popctrl-graph-id)
	       (variance event-popctrl-variance)	       
	       (durs event-popctrl-durs '())
	       (exclude event-popctrl-exclude '())
	       (method event-popctrl-method 'triloop)
	       (pgrow event-popctrl-grow-probability 10)
	       (pprune event-popctrl-prune-probability 10)
	       (phoedge event-popctrl-higher-order-probability 10)
	       (hoedge-max event-popctrl-higher-order-max-order 4))
  :direct-parameters (graph-id variance pgrow pprune method)
  :handler (incudine:nrt-funcall
	    (handler-case 
	        (let ((resolved-id (if (eql (event-popctrl-graph-id evt) 'self)
				       (car (event-source evt))
				       (event-popctrl-graph-id evt))))
		  (when (< (random 100) (event-popctrl-grow-probability evt))
		    (let ((order (if
				  (< (random 100)
				     (event-popctrl-higher-order-probability evt))
				  (+ 2 (random
					(- (event-popctrl-higher-order-max-order
					    evt) 2)))
				  nil)))
		      (grow resolved-id
			:variance (event-popctrl-variance evt)
			:durs (event-popctrl-durs evt)
			:method (event-popctrl-method evt)
			:higher-order order)))		  
		  (when (< (random 100) (event-popctrl-prune-probability evt))
		    (prune resolved-id
			   :exclude (event-popctrl-exclude evt))))	      
	      (simple-error (e)
		(incudine::msg
		 error "something went wrong executing growth:~% ~D" e)))))

(define-event
  :long-name growth-event
  :short-name growth
  :abstract-event nil
  :parent-events (event)
  :parameters ((graph-id event-growth-graph-id)
	       (variance event-growth-variance)	       
	       (durs event-growth-durs '())
	       (method event-growth-method 'triloop)
	       (higher-order event-growth-higher-order 4))
  :direct-parameters (graph-id variance)
  :handler (incudine:nrt-funcall
	    (handler-case 
	        (let ((resolved-id (if (eql (event-growth-graph-id evt) 'self)
				       (car (event-source evt))
				       (event-growth-graph-id evt))))
		  (grow resolved-id
			:variance (event-growth-variance evt)		        
			:durs (event-growth-durs evt)
			:method (event-growth-method evt)
			:higher-order (event-growth-higher-order evt)))
	      (simple-error (e)
		(incudine::msg
		 error "something went wrong executing growth:~% ~D" e)))))

(define-event
  :long-name shrink-event
  :short-name shrink
  :abstract-event nil
  :parent-events (event)
  :parameters ((graph-id event-shrink-graph-id)
	       (exclude event-shrink-exclude '())
	       (node-id event-shrink-node-id nil))
  :direct-parameters (graph-id)
  :handler (incudine:nrt-funcall
	    (handler-case 
	        (let ((resolved-id (if (eql (event-shrink-graph-id evt) 'self)
				       (car (event-source evt))
				       (event-shrink-graph-id evt))))
		  (prune resolved-id
			 :exclude (event-shrink-exclude evt)
			 :node-id (event-shrink-node-id evt)))
	      (simple-error (e)
		(incudine::msg
		 error "something went wrong executing shrink:~% ~D" e)))))

(define-event
  :long-name stack-push-event
  :short-name stack-push
  :parent-events (event)
  :parameters ((chain-id event-stack-push-chain-id 'self)
	       (variance event-stack-push-variance 0.001)
	       (shift event-stack-push-shift 0))
  :direct-parameters (chain-id variance)
  :handler (incudine:nrt-funcall
	    (handler-case 
	        (let ((resolved-id (if (eql (event-stack-push-chain-id evt) 'self)
				       (let ((graph-id (car (event-source evt))))
					 (chain-bound
					  (gethash graph-id *processor-directory*)))
				       (event-stack-push-chain-id evt))))
		  (branch resolved-id
			  :variance (event-stack-push-variance evt)
			  :shift (event-stack-push-variance evt)))
	      (simple-error (e)
		(incudine::msg
		 error "something went wrong executing stack-push ~D" e)))))

(define-event
  :long-name stack-pop-event
  :short-name stack-pop
  :parent-events (event)
  :parameters ((chain-id event-stack-pop-chain-id))
  :handler (incudine:nrt-funcall
	    (handler-case 
	        (let ((resolved-id (if (eql (event-stack-push-chain-id evt) 'self)
				       (let ((graph-id (car (event-source evt))))
					 (chain-bound
					  (gethash graph-id *processor-directory*)))
				       (event-stack-push-chain-id evt))))
		  (dq resolved-id))
	      (simple-error (e)
		(incudine::msg
		 error "something went wrong executing stack-pop ~D" e)))))

;; the transition between events is just a different type of event,
;; if you ask me ... 
(define-event
  :long-name transition-event
  :short-name transition
  :parent-events (event)
  :parameters ((dur transition-duration 512 20)) 
  :direct-parameters (dur))


