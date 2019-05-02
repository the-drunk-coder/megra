(in-package :megra)

(defmethod handle-grain-event-sc-8ch ((g grain-event-8ch) timestamp &key)
  (unless (gethash (grain-8ch-sample-location g) *sc-buffer-directory*)
    (register-sample (grain-8ch-sample-location g)))
  ;; might save a hashtable access here ... later ...
  (let ((bufnum (gethash (grain-8ch-sample-location g) *sc-buffer-directory*)))    
    (if (> (event-reverb g) 0)             
	(osc:simple-bundle cm::*oscout* timestamp  
			   "/s_new"	    
			   "siiisisfsfsfsfsfsfsfsfsfsfsfsfsfsfsfsfsfsfsf"
			   "grain_8ch_rev" -1 0 1
			   "bufnum" bufnum
			   "lvl" (coerce (event-level g) 'float)
			   "rate" (coerce (event-rate g) 'float)
			   "start" (coerce (event-start g) 'float)
			   "lp_freq" (coerce (event-lp-freq g) 'float)
			   "lp_q" (coerce (event-lp-q g) 'float)
			   "lp_dist" (coerce (event-lp-dist g) 'float)
			   "lp_freq_lfo_freq" (coerce (event-lp-freq-lfo-speed g) 'float)
			   "lp_freq_lfo_depth" (coerce (event-lp-freq-lfo-depth g) 'float)
			   "lp_freq_lfo_phase" (coerce (event-lp-freq-lfo-phase g) 'float)
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
			   "siiisisfsfsfsfsfsfsfsfsfsfsfsfsfsfsfsfsfsfsf"
			   "grain_8ch" -1 0 1
			   "bufnum" bufnum
			   "lvl" (coerce (event-level g) 'float)
			   "rate" (coerce (event-rate g) 'float)
			   "start" (coerce (event-start g) 'float)
			   "lp_freq" (coerce (event-lp-freq g) 'float)
			   "lp_q" (coerce (event-lp-q g) 'float)
			   "lp_dist" (coerce (event-lp-dist g) 'float)
			   "lp_freq_lfo_freq" (coerce (event-lp-freq-lfo-speed g) 'float)
			   "lp_freq_lfo_depth" (coerce (event-lp-freq-lfo-depth g) 'float)
			   "lp_freq_lfo_phase" (coerce (event-lp-freq-lfo-phase g) 'float)
			   "pf_freq" (coerce (event-pf-freq g) 'float)
			   "pf_q" (coerce (event-pf-q g) 'float)
			   "pf_gain" (coerce (event-pf-gain g) 'float)
			   "hp_freq" (coerce (event-hp-freq g) 'float)
			   "hp_q" (coerce (event-hp-q g)  'float)
			   "a" (coerce (* (event-attack g) 0.001) 'float)
			   "length" (coerce (* (- (event-duration g) (event-attack g) (event-release g)) 0.001) 'float)
			   "r" (coerce (* (event-release g) 0.001) 'float)
			   "pos" (coerce (- (event-position g) 0.5) 'float)))))

(defmethod handle-sine-event-sc-8ch ((s sine-event) timestamp &key)
  (if (> (event-reverb s) 0)     
      (osc:simple-bundle cm::*oscout* timestamp  
			 "/s_new"	    
			 "siiisfsfsfsfsfsfsfsfsfsf"
			 "sine_8ch_rev" -1 0 1
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
			 "sine_8ch" -1 0 1
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

(defmethod handle-triangle-event-sc-8ch ((s triangle-event) timestamp &key)
  (if (> (event-reverb s) 0)     
      (osc:simple-bundle cm::*oscout* timestamp  
			 "/s_new"	    
			 "siiisfsfsfsfsfsfsfsfsfsf"
			 "tri_8ch_rev" -1 0 1
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
			 "tri_8ch" -1 0 1
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

(defmethod handle-cub-event-sc-8ch ((s cub-event) timestamp &key)
  (if (> (event-reverb s) 0)     
      (osc:simple-bundle cm::*oscout* timestamp  
			 "/s_new"	    
			 "siiisfsfsfsfsfsfsfsfsfsf"
			 "cub_8ch_rev" -1 0 1
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
			 "cub_8ch" -1 0 1
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

(defmethod handle-par-event-sc-8ch ((s par-event) timestamp &key)
  (if (> (event-reverb s) 0)     
      (osc:simple-bundle cm::*oscout* timestamp  
			 "/s_new"	    
			 "siiisfsfsfsfsfsfsfsfsfsf"
			 "par_8ch_rev" -1 0 1
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
			 "par_8ch" -1 0 1
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

(defmethod handle-saw-event-sc-8ch ((s saw-event) timestamp &key)
  (if (> (event-reverb s) 0)     
      (osc:simple-bundle cm::*oscout* timestamp  
			 "/s_new"	    
			 "siiisfsfsfsfsfsfsfsfsfsf"
			 "saw_8ch_rev" -1 0 1
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
			 "saw_8ch" -1 0 1
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

(defmethod handle-square-event-sc-8ch ((s square-event) timestamp &key)
  (if (> (event-reverb s) 0)     
      (osc:simple-bundle cm::*oscout* timestamp  
			 "/s_new"	    
			 "siiisfsfsfsfsfsfsfsfsfsfsf"
			 "sqr_8ch_rev" -1 0 1
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
			 "rev" (coerce (event-reverb s) 'float)
			 "pulsewidth" (coerce (event-pulsewidth s) 'float))
      (osc:simple-bundle cm::*oscout* timestamp
			 "/s_new"	    
			 "siiisfsfsfsfsfsfsfsfsfsf"
			 "sqr_8ch" -1 0 1
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
			 "pulsewidth" (coerce (event-pulsewidth s) 'float)
			 )))


(defmethod handle-buzz-event-sc-8ch ((b buzz-event) timestamp &key)
  (if (> (event-reverb b) 0)     
      (osc:simple-bundle cm::*oscout* timestamp  
			 "/s_new"	    
			 "siiisfsfsfsfsfsfsfsfsfsfsf"
			 "buzz_8ch_rev" -1 0 1
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
			 "buzz_8ch" -1 0 1
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




(defmethod handle-pluck-event-sc-8ch ((p pluck-event) timestamp &key)
  (if (> (event-reverb p) 0)     
      (osc:simple-bundle cm::*oscout* timestamp  
			 "/s_new"	    
			 "siiisfsfsfsfsfsfsfsf"
			 "pluck_8ch_rev" -1 0 1
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
			 "pluck_8ch" -1 0 1
			 "freq" (coerce (if (typep (event-pitch p) 'symbol)
					    (cm::hertz (event-pitch p))
					    (event-pitch p)) 'float)
			 "lvl" (coerce (event-level p) 'float)	    
			 "lp_freq" (coerce (event-lp-freq p) 'float)
			 "lp_q" (coerce (event-lp-q p) 'float)
			 "lp_dist" (coerce (event-lp-dist p) 'float)	    
			 "length" (coerce (* (event-duration p) 0.001) 'float)
			 "pos" (coerce (- (event-position p) 0.5) 'float))))

(defmethod handle-risset-event-sc-8ch ((r risset-event) timestamp &key)
  (if (> (event-reverb r) 0)     
      (osc:simple-bundle cm::*oscout* timestamp  
			 "/s_new"	    
			 "siiisfsfsfsfsfsfsfsfsfsfsf"
			 "risset_8ch_rev" -1 0 1
			 "freq" (coerce (if (typep (event-pitch r) 'symbol)
					    (cm::hertz (event-pitch r))
					    (event-pitch r)) 'float)
			 "lvl" (coerce (event-level r) 'float)	    
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
			 "risset_8ch" -1 0 1
			 "freq" (coerce (if (typep (event-pitch r) 'symbol)
					    (cm::hertz (event-pitch r))
					    (event-pitch r)) 'float)
			 "lvl" (coerce (event-level r) 'float)	    
			 "lp_freq" (coerce (event-lp-freq r) 'float)
			 "lp_q" (coerce (event-lp-q r) 'float)
			 "lp_dist" (coerce (event-lp-dist r) 'float)	    
			 "a" (coerce (* (event-attack r) 0.001) 'float)
			 "d" (coerce (* (event-decay r) 0.001) 'float)
			 "s" (coerce (* (event-sustain r) 0.001) 'float)
			 "r" (coerce (* (event-release r) 0.001) 'float)
			 "pos" (coerce (- (event-position r) 0.5) 'float))))

(defmethod handle-dx-rhodes-event-sc-8ch ((d dx-rhodes-event) timestamp &key)
  (if (> (event-reverb d) 0)     
      (osc:simple-bundle cm::*oscout* timestamp  
			 "/s_new"	    
			 "siiisfsfsfsfsfsfsfsfsf"
			 "rhodey_sc_8ch_rev" -1 0 1
			 "freq" (coerce (if (typep (event-pitch d) 'symbol)
					    (cm::hertz (event-pitch d))
					    (event-pitch d)) 'float)			 
			 "lvl" (coerce (event-level d) 'float)
			 
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
			 "rhodey_sc_8ch" -1 0 1
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
