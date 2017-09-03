;; handler method for grain event, supercollider
(in-package :megra)
(defmethod handle-grain-event-sc ((g grain-event) timestamp &key)
  (unless (gethash (event-sample-location g) *sc-buffer-directory*)
    (register-sample (event-sample-location g)))
  ;; might save a hashtable access here ... later ...
  (let ((bufnum (gethash (event-sample-location g) *sc-buffer-directory*)))    
    (if (> (event-reverb g) 0)     
	(osc:simple-bundle cm::*oscout* timestamp  
			   "/s_new"	    
			   "siiisisfsfsfsfsfsfsfsfsfsfsfsfsfsfsfsf"
			   "grain_2ch_rev" -1 0 1
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
			   "pos" (coerce (- (event-position g) 0.5) 'float)
			   "rev" (coerce (event-reverb g) 'float))
	(osc:simple-bundle cm::*oscout* timestamp
			   "/s_new"	    
			   "siiisisfsfsfsfsfsfsfsfsfsfsfsfsfsfsfsf"
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
			   "pos" (coerce (- (event-position g) 0.5) 'float)))))

(defmethod handle-sine-event-sc ((s sine-event) timestamp &key)
  (if (> (event-reverb s) 0)     
      (osc:simple-bundle cm::*oscout* timestamp  
			 "/s_new"	    
			 "siiisfsfsfsfsfsfsfsfsfsf"
			 "sine_2ch_rev" -1 0 1
			 "freq" (coerce (if (typep (event-pitch s) 'symbol)
				    (cm::hertz (event-pitch s))
				    (event-pitch s)) 'float)			 
			 "lvl" (coerce (event-level s) 'float)
			 "lp_freq" (coerce (event-lp-freq s) 'float)
			 "lp_q" (coerce (event-lp-q s) 'float)
			 "lp_dist" (coerce (event-lp-dist s) 'float)	    
			 "a" (coerce (* (event-attack s) 0.001) 'float)
			 "length" (coerce (* (- (event-duration s) (event-attack s) (event-release s)) 0.001) 'float)
			 "r" (coerce (* (event-release s) 0.001) 'float)
			 "pos" (coerce (- (event-position s) 0.5) 'float)
			 "rev" (coerce (event-reverb s) 'float))
      (osc:simple-bundle cm::*oscout* timestamp
			 "/s_new"	    
			 "siiisfsfsfsfsfsfsfsfsf"
			 "sine_2ch" -1 0 1
			 "freq" (coerce (if (typep (event-pitch s) 'symbol)
				    (cm::hertz (event-pitch s))
				    (event-pitch s)) 'float)
			 "lvl" (coerce (event-level s) 'float)	    
			 "lp_freq" (coerce (event-lp-freq s) 'float)
			 "lp_q" (coerce (event-lp-q s) 'float)
			 "lp_dist" (coerce (event-lp-dist s) 'float)	    
			 "a" (coerce (* (event-attack s) 0.001) 'float)
			 "length" (coerce (* (- (event-duration s) (event-attack s) (event-release s)) 0.001) 'float)
			 "r" (coerce (* (event-release s) 0.001) 'float)
			 "pos" (coerce (- (event-position s) 0.5) 'float)
			 )))

(defmethod handle-buzz-event-sc ((b buzz-event) timestamp &key)
  (if (> (event-reverb b) 0)     
      (osc:simple-bundle cm::*oscout* timestamp  
			 "/s_new"	    
			 "siiisfsfsfsfsfsfsfsfsfsfsf"
			 "buzz_2ch_rev" -1 0 1
			 "freq" (coerce (if (typep (event-pitch b) 'symbol)
				    (cm::hertz (event-pitch b))
				    (event-pitch b)) 'float)			 
			 "lvl" (coerce (event-level b) 'float)	    
			 "lp_freq" (coerce (event-lp-freq b) 'float)
			 "lp_q" (coerce (event-lp-q b) 'float)
			 "lp_dist" (coerce (event-lp-dist b) 'float)	    
			 "a" (coerce (* (event-attack b) 0.001) 'float)
			 "length" (coerce (* (- (event-duration b) (event-attack b) (event-release b)) 0.001) 'float)
			 "r" (coerce (* (event-release b) 0.001) 'float)
			 "pos" (coerce (- (event-position b) 0.5) 'float)
			 "rev" (coerce (event-reverb b) 'float)
			 "harm" (coerce (event-harmonies b) 'float))
      (osc:simple-bundle cm::*oscout* timestamp
			 "/s_new"	    
			 "siiisfsfsfsfsfsfsfsfsfsf"
			 "buzz_2ch" -1 0 1
			 "freq" (coerce (if (typep (event-pitch b) 'symbol)
				    (cm::hertz (event-pitch b))
				    (event-pitch b)) 'float)
			 "lvl" (coerce (event-level b) 'float)	    
			 "lp_freq" (coerce (event-lp-freq b) 'float)
			 "lp_q" (coerce (event-lp-q b) 'float)
			 "lp_dist" (coerce (event-lp-dist b) 'float)	    
			 "a" (coerce (* (event-attack b) 0.001) 'float)
			 "length" (coerce (* (- (event-duration b) (event-attack b) (event-release b)) 0.001) 'float)
			 "r" (coerce (* (event-release b) 0.001) 'float)
			 "pos" (coerce (- (event-position b) 0.5) 'float)
			 "harm" (coerce (event-harmonies b) 'float))))

(defmethod handle-square-event-sc ((s square-event) timestamp &key)
  (if (> (event-reverb s) 0)     
      (osc:simple-bundle cm::*oscout* timestamp  
			 "/s_new"	    
			 "siiisfsfsfsfsfsfsfsfsfsfsfsf"
			 "sqr_2ch_rev" -1 0 1
			 "freq" (coerce (if (typep (event-pitch s) 'symbol)
				    (cm::hertz (event-pitch s))
				    (event-pitch s)) 'float)			 
			 "gain" (coerce (event-level s) 'float)	    
			 "lp_freq" (coerce (event-lp-freq s) 'float)
			 "lp_q" (coerce (event-lp-q s) 'float)
			 "lp_dist" (coerce (event-lp-dist s) 'float)	    
			 "a" (coerce (* (event-attack s) 0.001) 'float)
			 "d" (coerce (* (event-decay s) 0.001) 'float)
			 "s" (coerce (* (event-sustain s) 0.001) 'float)
			 "r" (coerce (* (event-release s) 0.001) 'float)
			 "pos" (coerce (- (event-position s) 0.5) 'float)
			 "rev" (coerce (event-reverb s) 'float)
			 "pulsewidth" (coerce (event-pulsewidth s) 'float))
      (osc:simple-bundle cm::*oscout* timestamp
			 "/s_new"	    
			 "siiisfsfsfsfsfsfsfsfsfsfsf"
			 "sqr_2ch" -1 0 1
			 "freq" (coerce (if (typep (event-pitch s) 'symbol)
				    (cm::hertz (event-pitch s))
				    (event-pitch s)) 'float)
			 "gain" (coerce (event-level s) 'float)	    
			 "lp_freq" (coerce (event-lp-freq s) 'float)
			 "lp_q" (coerce (event-lp-q s) 'float)
			 "lp_dist" (coerce (event-lp-dist s) 'float)	    
			 "a" (coerce (* (event-attack s) 0.001) 'float)
			 "d" (coerce (* (event-decay s) 0.001) 'float)
			 "s" (coerce (* (event-sustain s) 0.001) 'float)
			 "r" (coerce (* (event-release s) 0.001) 'float)
			 "pos" (coerce (- (event-position s) 0.5) 'float)
			 "pulsewidth" (coerce (event-pulsewidth s) 'float))))

(defmethod handle-saw-event-sc ((s saw-event) timestamp &key)
  (if (> (event-reverb s) 0)     
      (osc:simple-bundle cm::*oscout* timestamp  
			 "/s_new"	    
			 "siiisfsfsfsfsfsfsfsfsfsfsfsf"
			 "saw_2ch_rev" -1 0 1
			 "freq" (coerce (if (typep (event-pitch s) 'symbol)
				    (cm::hertz (event-pitch s))
				    (event-pitch s)) 'float)			 
			 "gain" (coerce (event-level s) 'float)	    
			 "lp_freq" (coerce (event-lp-freq s) 'float)
			 "lp_q" (coerce (event-lp-q s) 'float)
			 "lp_dist" (coerce (event-lp-dist s) 'float)	    
			 "a" (coerce (* (event-attack s) 0.001) 'float)
			 "d" (coerce (* (event-decay s) 0.001) 'float)
			 "s" (coerce (* (event-sustain s) 0.001) 'float)
			 "r" (coerce (* (event-release s) 0.001) 'float)
			 "pos" (coerce (- (event-position s) 0.5) 'float)
			 "rev" (coerce (event-reverb s) 'float))
      (osc:simple-bundle cm::*oscout* timestamp
			 "/s_new"	    
			 "siiisfsfsfsfsfsfsfsfsfsfsf"
			 "saw_2ch" -1 0 1
			 "freq" (coerce (if (typep (event-pitch s) 'symbol)
				    (cm::hertz (event-pitch s))
				    (event-pitch s)) 'float)
			 "gain" (coerce (event-level s) 'float)	    
			 "lp_freq" (coerce (event-lp-freq s) 'float)
			 "lp_q" (coerce (event-lp-q s) 'float)
			 "lp_dist" (coerce (event-lp-dist s) 'float)	    
			 "a" (coerce (* (event-attack s) 0.001) 'float)
			 "d" (coerce (* (event-decay s) 0.001) 'float)
			 "s" (coerce (* (event-sustain s) 0.001) 'float)
			 "r" (coerce (* (event-release s) 0.001) 'float)
			 "pos" (coerce (- (event-position s) 0.5) 'float))))


(defmethod handle-meow-event-sc ((m meow-event) timestamp &key)
  (if (> (event-reverb m) 0)     
      (osc:simple-bundle cm::*oscout* timestamp  
			 "/s_new"	    
			 "siiisfsfsfsfsfsfsfsfsfsfsf"
			 "meow_2ch_rev" -1 0 1
			 "freq" (coerce (if (typep (event-pitch m) 'symbol)
				    (cm::hertz (event-pitch m))
				    (event-pitch m)) 'float)			 
			 "gain" (coerce (event-level m) 'float)	    
			 "lp_freq" (coerce (event-lp-freq m) 'float)
			 "lp_q" (coerce (event-lp-q m) 'float)
			 "lp_dist" (coerce (event-lp-dist m) 'float)	    
			 "a" (coerce (* (event-attack m) 0.001) 'float)
			 "d" (coerce (* (event-decay m) 0.001) 'float)
			 "s" (coerce (* (event-sustain m) 0.001) 'float)
			 "r" (coerce (* (event-release m) 0.001) 'float)
			 "pos" (coerce (- (event-position m) 0.5) 'float)
			 "rev" (coerce (event-reverb m) 'float))
      (osc:simple-bundle cm::*oscout* timestamp
			 "/s_new"	    
			 "siiisfsfsfsfsfsfsfsfsfsf"
			 "meow_2ch" -1 0 1
			 "freq" (coerce (if (typep (event-pitch m) 'symbol)
				    (cm::hertz (event-pitch m))
				    (event-pitch m)) 'float)
			 "gain" (coerce (event-level m) 'float)	    
			 "lp_freq" (coerce (event-lp-freq m) 'float)
			 "lp_q" (coerce (event-lp-q m) 'float)
			 "lp_dist" (coerce (event-lp-dist m) 'float)	    
			 "a" (coerce (* (event-attack m) 0.001) 'float)
			 "d" (coerce (* (event-decay m) 0.001) 'float)
			 "s" (coerce (* (event-sustain m) 0.001) 'float)
			 "r" (coerce (* (event-release m) 0.001) 'float)
			 "pos" (coerce (- (event-position m) 0.5) 'float))))


(defmethod handle-pluck-event-sc ((p pluck-event) timestamp &key)
  (if (> (event-reverb p) 0)     
      (osc:simple-bundle cm::*oscout* timestamp  
			 "/s_new"	    
			 "siiisfsfsfsfsfsfsfsf"
			 "pluck_2ch_rev" -1 0 1
			 "freq" (coerce (if (typep (event-pitch p) 'symbol)
				    (cm::hertz (event-pitch p))
				    (event-pitch p)) 'float)			 
			 "lvl" (coerce (event-level p) 'float)	    
			 "lp_freq" (coerce (event-lp-freq p) 'float)
			 "lp_q" (coerce (event-lp-q p) 'float)
			 "lp_dist" (coerce (event-lp-dist p) 'float)	    
			 "length" (coerce (* (event-duration p) 0.001) 'float)	 
			 "pos" (coerce (- (event-position p) 0.5) 'float)
			 "rev" (coerce (event-reverb p) 'float))
      (osc:simple-bundle cm::*oscout* timestamp
			 "/s_new"	    
			 "siiisfsfsfsfsfsfsf"
			 "pluck_2ch" -1 0 1
			 "freq" (coerce (if (typep (event-pitch p) 'symbol)
				    (cm::hertz (event-pitch p))
				    (event-pitch p)) 'float)
			 "gain" (coerce (event-level p) 'float)	    
			 "lp_freq" (coerce (event-lp-freq p) 'float)
			 "lp_q" (coerce (event-lp-q p) 'float)
			 "lp_dist" (coerce (event-lp-dist p) 'float)	    
			 "length" (coerce (* (event-duration p) 0.001) 'float)
			 "pos" (coerce (- (event-position p) 0.5) 'float))))

(defmethod handle-risset-event-sc ((r risset-event) timestamp &key)
  (if (> (event-reverb r) 0)     
      (osc:simple-bundle cm::*oscout* timestamp  
			 "/s_new"	    
			 "siiisfsfsfsfsfsfsfsfsfsfsf"
			 "risset_2ch_rev" -1 0 1
			 "freq" (coerce (if (typep (event-pitch r) 'symbol)
					    (cm::hertz (event-pitch r))
					    (event-pitch r)) 'float)
			 "gain" (coerce (event-level r) 'float)	    
			 "lp_freq" (coerce (event-lp-freq r) 'float)
			 "lp_q" (coerce (event-lp-q r) 'float)
			 "lp_dist" (coerce (event-lp-dist r) 'float)	    
			 "a" (coerce (* (event-attack r) 0.001) 'float)
			 "d" (coerce (* (event-decay r) 0.001) 'float)
			 "s" (coerce (* (event-sustain r) 0.001) 'float)
			 "r" (coerce (* (event-release r) 0.001) 'float)
			 "pos" (coerce (- (event-position r) 0.5) 'float)
			 "rev" (coerce (event-reverb r) 'float))
      (osc:simple-bundle cm::*oscout* timestamp
			 "/s_new"	    
			 "siiisfsfsfsfsfsfsfsfsfsf"
			 "risset_2ch" -1 0 1
			 "freq" (coerce (if (typep (event-pitch r) 'symbol)
					    (cm::hertz (event-pitch r))
					    (event-pitch r)) 'float)
			 "gain" (coerce (event-level r) 'float)	    
			 "lp_freq" (coerce (event-lp-freq r) 'float)
			 "lp_q" (coerce (event-lp-q r) 'float)
			 "lp_dist" (coerce (event-lp-dist r) 'float)	    
			 "a" (coerce (* (event-attack r) 0.001) 'float)
			 "d" (coerce (* (event-decay r) 0.001) 'float)
			 "s" (coerce (* (event-sustain r) 0.001) 'float)
			 "r" (coerce (* (event-release r) 0.001) 'float)
			 "pos" (coerce (- (event-position r) 0.5) 'float))))

(defmethod handle-dx-rhodes-event-sc ((d dx-rhodes-event) timestamp &key)
  (if (> (event-reverb d) 0)     
      (osc:simple-bundle cm::*oscout* timestamp  
			 "/s_new"	    
			 "siiisfsfsfsfsfsfsfsfsf"
			 "rhodey_sc_2ch_rev" -1 0 1
			 "freq" (coerce (if (typep (event-pitch d) 'symbol)
				    (cm::hertz (event-pitch d))
				    (event-pitch d)) 'float)			 
			 "gain" (coerce (event-level d) 'float)
			 
			 "mix" (coerce (event-mix d) 'float)	    
			 "lfoSpeed" (coerce (event-level-lfo-speed d) 'float)
			 "lfoDepth" (coerce (event-level-lfo-depth d) 'float)
			 "modIndex" (coerce (event-mod-index d) 'float)
			 "vel" (coerce (event-velocity d) 'float)
			 "pos" (coerce (- (event-position d) 0.5) 'float)
			 "rev" (coerce (event-reverb d) 'float))
      (osc:simple-bundle cm::*oscout* timestamp
			 "/s_new"	    
			 "siiisfsfsfsfsfsfsfsf"
			 "rhodey_sc_2ch" -1 0 1
			 "freq" (coerce (if (typep (event-pitch d) 'symbol)
				    (cm::hertz (event-pitch d))
				    (event-pitch d)) 'float)
			 "gain" (coerce (event-level d) 'float)	    
			 "mix" (coerce (event-mix d) 'float)	    
			 "lfoSpeed" (coerce (event-level-lfo-speed d) 'float)
			 "lfoDepth" (coerce (event-level-lfo-depth d) 'float)
			 "modIndex" (coerce (event-mod-index d) 'float)
			 "vel" (coerce (event-velocity d) 'float)			 
			 "pos" (coerce (- (event-position d) 0.5) 'float))))
