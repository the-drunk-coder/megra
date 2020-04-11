(in-package :megra)

(defmethod handle-grain-event-sc ((g grain-event) timestamp &key)
  (unless (gethash (grain-sample-location g) *sc-buffer-directory*)
    (register-sample (grain-sample-location g)))
  ;; might save a hashtable access here ... later ...
  (let ((bufnum (gethash (grain-sample-location g) *sc-buffer-directory*)))    
    (osc:simple-bundle cm::*oscout* timestamp  
		       "/s_new"	    
		       "siiisisfsfsfsfsfsfsfsfsfsfsfsfsfsfsfsfsfsfsfsfsf"
		       "grain_2ch" -1 0 1
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
		       "pos" (coerce (event-position g) 'float)
		       "rev" (coerce (event-reverb g) 'float)
                       "echo" (coerce (event-echo g) 'float)
                       "echorev" (coerce (event-echorev g) 'float))))

(defmethod handle-grain-event-sc-nores ((g grain-event-nores) timestamp &key)
  (unless (gethash (nores-sample-location g) *sc-buffer-directory*)
    (register-sample (nores-sample-location g)))
  ;; might save a hashtable access here ... later ...
  (let ((bufnum (gethash (nores-sample-location g) *sc-buffer-directory*)))             
    (osc:simple-bundle cm::*oscout* timestamp  
		       "/s_new"	    
		       "siiisisfsfsfsfsfsfsfsfsfsfsfsfsfsfsfsfsfsf"
		       "grain_2ch_nores" -1 0 1
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
		       "pos" (coerce (event-position g) 'float)
		       "rev" (coerce (event-reverb g) 'float)
                       "echo" (coerce (event-echo g) 'float)
                       "echorev" (coerce (event-echorev g) 'float))))

(defmethod handle-grain-event-sc-24db ((g grain-event-24db) timestamp &key)
  (unless (gethash (twofourdb-sample-location g) *sc-buffer-directory*)
    (register-sample (twofourdb-sample-location g)))
  ;; might save a hashtable access here ... later ...
  (let ((bufnum (gethash (twofourdb-sample-location g) *sc-buffer-directory*)))
    (osc:simple-bundle cm::*oscout* timestamp  
		       "/s_new"	    
		       "siiisisfsfsfsfsfsfsfsfsfsfsfsfsfsfsfsfsfsfsf"
		       "grain_2ch_24db" -1 0 1
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
		       "pos" (coerce (event-position g) 'float)
                       "rev" (coerce (event-reverb g) 'float)
                       "echo" (coerce (event-echo g) 'float)
		       "echorev" (coerce (event-echorev g) 'float))))

(defmethod handle-sine-event-sc ((s sine-event) timestamp &key)
  (osc:simple-bundle cm::*oscout* timestamp
		     "/s_new"	    
		     "siiisfsfsfsfsfsfsfsfsfsfsfsf"
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
		     "pos" (coerce (event-position s) 'float)
		     "rev" (coerce (event-reverb s) 'float)
                     "echo" (coerce (event-echo s) 'float)
                     "echorev" (coerce (event-echorev s) 'float)))

(defmethod handle-triangle-event-sc ((s triangle-event) timestamp &key)  
  (osc:simple-bundle cm::*oscout* timestamp  
		     "/s_new"	    
		     "siiisfsfsfsfsfsfsfsfsfsfsfsf"
		     "tri_2ch" -1 0 1
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
		     "pos" (coerce (event-position s) 'float)
		     "rev" (coerce (event-reverb s) 'float)
                     "echo" (coerce (event-echo s) 'float)
                     "echorev" (coerce (event-echorev s) 'float)))

(defmethod handle-cub-event-sc ((s cub-event) timestamp &key)
  (osc:simple-bundle cm::*oscout* timestamp  
		     "/s_new"	    
		     "siiisfsfsfsfsfsfsfsfsfsfsfsf"
		     "cub_2ch" -1 0 1
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
		     "pos" (coerce (event-position s) 'float)
		     "rev" (coerce (event-reverb s) 'float)
                     "echo" (coerce (event-echo s) 'float)
                     "echorev" (coerce (event-echorev s) 'float)))

(defmethod handle-par-event-sc ((s par-event) timestamp &key)  
  (osc:simple-bundle cm::*oscout* timestamp  
		     "/s_new"	    
		     "siiisfsfsfsfsfsfsfsfsfsfsfsf"
		     "par_2ch" -1 0 1
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
		     "pos" (coerce (event-position s) 'float)
		     "rev" (coerce (event-reverb s) 'float)
                     "echo" (coerce (event-echo s) 'float)
                     "echorev" (coerce (event-echorev s) 'float)))

(defmethod handle-saw-event-sc ((s saw-event) timestamp &key)  
  (osc:simple-bundle cm::*oscout* timestamp  
		     "/s_new"	    
		     "siiisfsfsfsfsfsfsfsfsfsfsfsf"
		     "saw_2ch" -1 0 1
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
		     "pos" (coerce (event-position s) 'float)
		     "rev" (coerce (event-reverb s) 'float)
                     "echo" (coerce (event-echo s) 'float)
                     "echorev" (coerce (event-echorev s) 'float)))

(defmethod handle-square-event-sc ((s square-event) timestamp &key)
  (osc:simple-bundle cm::*oscout* timestamp  
		     "/s_new"	    
		     "siiisfsfsfsfsfsfsfsfsfsfsfsfsf"
		     "sqr_2ch" -1 0 1
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
		     "pos" (coerce (event-position s) 'float)
		     "rev" (coerce (event-reverb s) 'float)
                     "echo" (coerce (event-echo s) 'float)
                     "echorev" (coerce (event-echorev s) 'float)
		     "pulsewidth" (coerce (event-pulsewidth s) 'float)))

(defmethod handle-buzz-event-sc ((b buzz-event) timestamp &key)
  (osc:simple-bundle cm::*oscout* timestamp  
		     "/s_new"	    
		     "siiisfsfsfsfsfsfsfsfsfsfsfsfsf"
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
		     "pos" (coerce (event-position b) 'float)
		     "rev" (coerce (event-reverb b) 'float)
                     "echo" (coerce (event-echo b) 'float)
                     "echorev" (coerce (event-echorev b) 'float)
		     "harm" (coerce (event-harmonies b) 'float)))

(defmethod handle-square-adsr-event-sc ((s square-adsr-event) timestamp &key)
  (osc:simple-bundle cm::*oscout* timestamp  
		     "/s_new"	    
		     "siiisfsfsfsfsfsfsfsfsfsfsfsfsfsf"
		     "sqr_2ch" -1 0 1
		     "freq" (coerce (if (typep (event-pitch s) 'symbol)
					(cm::hertz (event-pitch s))
					(event-pitch s)) 'float)			 
		     "lvl" (coerce (event-level s) 'float)	    
		     "lp_freq" (coerce (event-lp-freq s) 'float)
		     "lp_q" (coerce (event-lp-q s) 'float)
		     "lp_dist" (coerce (event-lp-dist s) 'float)	    
		     "a" (coerce (* (event-attack s) 0.001) 'float)
		     "d" (coerce (* (event-decay s) 0.001) 'float)
		     "s" (coerce (* (event-sustain s) 0.001) 'float)
		     "r" (coerce (* (event-release s) 0.001) 'float)
		     "pos" (coerce (event-position s) 'float)
		     "rev" (coerce (event-reverb s) 'float)
                     "echo" (coerce (event-echo s) 'float)
                     "echorev" (coerce (event-echorev s) 'float)
		     "pulsewidth" (coerce (event-pulsewidth s) 'float)))

(defmethod handle-saw-adsr-event-sc ((s saw-adsr-event) timestamp &key)
  (osc:simple-bundle cm::*oscout* timestamp  
		     "/s_new"	    
		     "siiisfsfsfsfsfsfsfsfsfsfsfsfsfsf"
		     "saw_2ch" -1 0 1
		     "freq" (coerce (if (typep (event-pitch s) 'symbol)
					(cm::hertz (event-pitch s))
					(event-pitch s)) 'float)			 
		     "lvl" (coerce (event-level s) 'float)	    
		     "lp_freq" (coerce (event-lp-freq s) 'float)
		     "lp_q" (coerce (event-lp-q s) 'float)
		     "lp_dist" (coerce (event-lp-dist s) 'float)	    
		     "a" (coerce (* (event-attack s) 0.001) 'float)
		     "d" (coerce (* (event-decay s) 0.001) 'float)
		     "s" (coerce (* (event-sustain s) 0.001) 'float)
		     "r" (coerce (* (event-release s) 0.001) 'float)
		     "pos" (coerce (event-position s) 'float)
		     "rev" (coerce (event-reverb s) 'float)
                     "echo" (coerce (event-echo s) 'float)
                     "echorev" (coerce (event-echorev s) 'float)))

(defmethod handle-meow-event-sc ((m meow-event) timestamp &key)
  (osc:simple-bundle cm::*oscout* timestamp  
		     "/s_new"	    
		     "siiisfsfsfsfsfsfsfsfsfsfsfsfsf"
		     "meow_2ch" -1 0 1
		     "freq" (coerce (if (typep (event-pitch m) 'symbol)
					(cm::hertz (event-pitch m))
					(event-pitch m)) 'float)			 
		     "lvl" (coerce (event-level m) 'float)	    
		     "lp_freq" (coerce (event-lp-freq m) 'float)
		     "lp_q" (coerce (event-lp-q m) 'float)
		     "lp_dist" (coerce (event-lp-dist m) 'float)	    
		     "a" (coerce (* (event-attack m) 0.001) 'float)
		     "d" (coerce (* (event-decay m) 0.001) 'float)
		     "s" (coerce (* (event-sustain m) 0.001) 'float)
		     "r" (coerce (* (event-release m) 0.001) 'float)
		     "pos" (coerce (event-position m) 'float)
		     "rev" (coerce (event-reverb m) 'float)
                     "echo" (coerce (event-echo m) 'float)
                     "echorev" (coerce (event-echorev m) 'float)))


(defmethod handle-pluck-event-sc ((p pluck-event) timestamp &key)
  (osc:simple-bundle cm::*oscout* timestamp  
		     "/s_new"	    
		     "siiisfsfsfsfsfsfsfsfsfsf"
		     "pluck_2ch" -1 0 1
		     "freq" (coerce (if (typep (event-pitch p) 'symbol)
					(cm::hertz (event-pitch p))
					(event-pitch p)) 'float)			 
		     "lvl" (coerce (event-level p) 'float)	    
		     "lp_freq" (coerce (event-lp-freq p) 'float)
		     "lp_q" (coerce (event-lp-q p) 'float)
		     "lp_dist" (coerce (event-lp-dist p) 'float)	    
		     "length" (coerce (* (event-duration p) 0.001) 'float)	 
		     "pos" (coerce (event-position p) 'float)
		     "rev" (coerce (event-reverb p) 'float)
                     "echo" (coerce (event-echo p) 'float)
                     "echorev" (coerce (event-echorev p) 'float)))

(defmethod handle-risset-event-sc ((r risset-event) timestamp &key)

  (osc:simple-bundle cm::*oscout* timestamp  
		     "/s_new"	    
		     "siiisfsfsfsfsfsfsfsfsfsfsfsfsf"
		     "risset_2ch" -1 0 1
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
		     "pos" (coerce (event-position r) 'float)
		     "rev" (coerce (event-reverb r) 'float)
                     "echo" (coerce (event-echo r) 'float)
                     "echorev" (coerce (event-echorev r) 'float)))

(defmethod handle-dx-rhodes-event-sc ((d dx-rhodes-event) timestamp &key)
  (osc:simple-bundle cm::*oscout* timestamp  
		     "/s_new"	    
		     "siiisfsfsfsfsfsfsfsfsfsfsf"
		     "rhodey_sc_2ch" -1 0 1
		     "freq" (coerce (if (typep (event-pitch d) 'symbol)
					(cm::hertz (event-pitch d))
					(event-pitch d)) 'float)			 
		     "lvl" (coerce (event-level d) 'float)
		     
		     "mix" (coerce (event-mix d) 'float)	    
		     "lfoSpeed" (coerce (event-level-lfo-speed d) 'float)
		     "lfoDepth" (coerce (event-level-lfo-depth d) 'float)
		     "modIndex" (coerce (event-mod-index d) 'float)
		     "vel" (coerce (event-velocity d) 'float)
		     "pos" (coerce (event-position d) 'float)
		     "rev" (coerce (event-reverb d) 'float)
                     "echo" (coerce (event-echo d) 'float)
                     "echorev" (coerce (event-echorev d) 'float)))
