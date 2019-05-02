(in-package :megra)

(defmethod handle-grain-event-sc-ambi ((g grain-event-ambi) timestamp &key)
  (unless (gethash (grain-ambi-sample-location g) *sc-buffer-directory*)
    (register-sample (grain-ambi-sample-location g)))
  ;; might save a hashtable access here ... later ...
  (let ((bufnum (gethash (grain-ambi-sample-location g) *sc-buffer-directory*)))    
    (if (> (event-reverb g) 0)     
	(osc:simple-bundle cm::*oscout* timestamp  
			   "/s_new"	    
			   "siiisisfsfsfsfsfsfsfsfsfsfsfsfsfsfsfsfsfsfsfsfsf"
			   "grain_ambi_rev" -1 0 1
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
			   "azi" (coerce (* (event-azimuth g) 3.14159) 'float)
			   "ele" (coerce (* (event-elevation g) (* 3.14159 0.5)) 'float)
			   "radius" (coerce (* (event-distance g) 0.001) 'float)			       
			   "rev" (coerce (event-reverb g) 'float))       
	(osc:simple-bundle cm::*oscout* timestamp
			   "/s_new"	    
			   "siiisisfsfsfsfsfsfsfsfsfsfsfsfsfsfsfsfsfsfsfsfsf"
			   "grain_ambi" -1 0 1
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
			   "azi" (coerce (* (event-azimuth g) 3.14159) 'float)
			   "ele" (coerce (* (event-elevation g) (* 3.14159 0.5)) 'float)
			   "radius" (coerce (* (event-distance g) 0.001) 'float)))))

(defmethod handle-grain-event-sc-nores-ambi ((g grain-event-nores-ambi) timestamp &key)
  (unless (gethash (nores-ambi-sample-location g) *sc-buffer-directory*)
    (register-sample (nores-ambi-sample-location g)))
  ;; might save a hashtable access here ... later ...
  (let ((bufnum (gethash (nores-ambi-sample-location g) *sc-buffer-directory*)))    
    (if (> (event-reverb g) 0)        
	(osc:simple-bundle cm::*oscout* timestamp  
			   "/s_new"	    
			   "siiisisfsfsfsfsfsfsfsfsfsfsfsfsfsfsfsfsf"
			   "grain_ambi_rev_nores" -1 0 1
			   "bufnum" bufnum
			   "lvl" (coerce (event-level g) 'float)
			   "rate" (coerce (event-rate g) 'float)
			   "start" (coerce (event-start g) 'float)
			   "lp_freq" (coerce (event-lp-freq g) 'float)
			   "lp_freq_lfo_freq" (coerce (event-lp-freq-lfo-speed g) 'float)
			   "lp_freq_lfo_depth" (coerce (event-lp-freq-lfo-depth g) 'float)
			   "lp_freq_lfo_phase" (coerce (event-lp-freq-lfo-phase g) 'float)
			   "pf_freq" (coerce (event-pf-freq g) 'float)
			   "pf_q" (coerce (event-pf-q g) 'float)
			   "pf_gain" (coerce (event-pf-gain g) 'float)
			   "hp_freq" (coerce (event-hp-freq g) 'float)
			   "a" (coerce (* (event-attack g) 0.001) 'float)
			   "length" (coerce (* (- (event-duration g) (event-attack g) (event-release g)) 0.001) 'float)
			   "r" (coerce (* (event-release g) 0.001) 'float)
			   "azi" (coerce (* (event-azimuth g) 3.14159) 'float)
			   "ele" (coerce (* (event-elevation g) (* 3.14159 0.5)) 'float)			       
			   "rev" (coerce (event-reverb g) 'float))               
	(osc:simple-bundle cm::*oscout* timestamp
			   "/s_new"	    
			   "siiisisfsfsfsfsfsfsfsfsfsfsfsfsfsfsfsf"
			   "grain_ambi_nores" -1 0 1
			   "bufnum" bufnum
			   "lvl" (coerce (event-level g) 'float)
			   "rate" (coerce (event-rate g) 'float)
			   "start" (coerce (event-start g) 'float)
			   "lp_freq" (coerce (event-lp-freq g) 'float)			       
			   "lp_freq_lfo_freq" (coerce (event-lp-freq-lfo-speed g) 'float)
			   "lp_freq_lfo_depth" (coerce (event-lp-freq-lfo-depth g) 'float)
			   "lp_freq_lfo_phase" (coerce (event-lp-freq-lfo-phase g) 'float)
			   "pf_freq" (coerce (event-pf-freq g) 'float)
			   "pf_q" (coerce (event-pf-q g) 'float)
			   "pf_gain" (coerce (event-pf-gain g) 'float)
			   "hp_freq" (coerce (event-hp-freq g) 'float)
			   "a" (coerce (* (event-attack g) 0.001) 'float)
			   "length" (coerce (* (- (event-duration g) (event-attack g) (event-release g)) 0.001) 'float)
			   "r" (coerce (* (event-release g) 0.001) 'float)
			   "azi" (coerce (* (event-azimuth g) 3.14159) 'float)
			   "ele" (coerce (* (event-elevation g) (* 3.14159 0.5)) 'float)))))

(defmethod handle-grain-event-sc-24db-ambi ((g grain-event-24db-ambi) timestamp &key)
  (unless (gethash (twofourdb-ambi-sample-location g) *sc-buffer-directory*)
    (register-sample (twofourdb-ambi-sample-location g)))
  ;; might save a hashtable access here ... later ...
  (let ((bufnum (gethash (twofourdb-ambi-sample-location g) *sc-buffer-directory*)))    
    (if (> (event-reverb g) 0)     
	(osc:simple-bundle cm::*oscout* timestamp  
			   "/s_new"	    
			   "siiisisfsfsfsfsfsfsfsfsfsfsfsfsfsfsfsfsfsf"
			   "grain_ambi_rev_24db" -1 0 1
			   "bufnum" bufnum
			   "lvl" (coerce (event-level g) 'float)
			   "rate" (coerce (event-rate g) 'float)
			   "start" (coerce (event-start g) 'float)
			   "lp_freq" (coerce (event-lp-freq g) 'float)
			   "lp_q" (coerce (event-lp-q g) 'float)
			   "lp_freq_lfo_freq" (coerce (event-lp-freq-lfo-speed g) 'float)
			   "lp_freq_lfo_depth" (coerce (event-lp-freq-lfo-depth g) 'float)
			   "lp_freq_lfo_phase" (coerce (event-lp-freq-lfo-phase g) 'float)
			   "pf_freq" (coerce (event-pf-freq g) 'float)
			   "pf_q" (coerce (event-pf-q g) 'float)
			   "pf_gain" (coerce (event-pf-gain g) 'float)
			   "hp_freq" (coerce (event-hp-freq g) 'float)
			   "a" (coerce (* (event-attack g) 0.001) 'float)
			   "length" (coerce (* (- (event-duration g) (event-attack g) (event-release g)) 0.001) 'float)
			   "r" (coerce (* (event-release g) 0.001) 'float)
			   "azi" (coerce (* (event-azimuth g) 3.14159) 'float)
			   "ele" (coerce (* (event-elevation g) (* 3.14159 0.5)) 'float)			       
			   "rev" (coerce (event-reverb g) 'float))        
	(osc:simple-bundle cm::*oscout* timestamp
			   "/s_new"	    
			   "siiisisfsfsfsfsfsfsfsfsfsfsfsfsfsfsfsfsf"
			   "grain_ambi_24db" -1 0 1
			   "bufnum" bufnum
			   "lvl" (coerce (event-level g) 'float)
			   "rate" (coerce (event-rate g) 'float)
			   "start" (coerce (event-start g) 'float)
			   "lp_freq" (coerce (event-lp-freq g) 'float)
			   "lp_q" (coerce (event-lp-q g) 'float)			       
			   "lp_freq_lfo_freq" (coerce (event-lp-freq-lfo-speed g) 'float)
			   "lp_freq_lfo_depth" (coerce (event-lp-freq-lfo-depth g) 'float)
			   "lp_freq_lfo_phase" (coerce (event-lp-freq-lfo-phase g) 'float)
			   "pf_freq" (coerce (event-pf-freq g) 'float)
			   "pf_q" (coerce (event-pf-q g) 'float)
			   "pf_gain" (coerce (event-pf-gain g) 'float)
			   "hp_freq" (coerce (event-hp-freq g) 'float)
			   "a" (coerce (* (event-attack g) 0.001) 'float)
			   "length" (coerce (* (- (event-duration g) (event-attack g) (event-release g)) 0.001) 'float)
			   "r" (coerce (* (event-release g) 0.001) 'float)
			   "azi" (coerce (* (event-azimuth g) 3.14159) 'float)
			   "ele" (coerce (* (event-elevation g) (* 3.14159 0.5)) 'float)))))

