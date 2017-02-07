(in-package :scratch)

(define-vug grain-core ((buf buffer)
			unit-rate
			frames
			gain
			rate
			start-pos
			lp-freq
			lp-q
			lp-dist
			peak-freq
			peak-q
			peak-gain
			hp-freq
			hp-q
			a
			length
			r)
  (lpf18
   (peak-eq 
    (hpf 	
     (* (envelope (make-local-envelope `(0 ,gain ,gain 0) `(,a ,length ,r) ) 1 1 #'free)
	(buffer-read buf (* (phasor (* rate unit-rate) start-pos) frames)
		     :wrap-p nil :interpolation :cubic))
     hp-freq hp-q)
    peak-freq peak-q peak-gain)
   lp-freq lp-q lp-dist))

(dsp! megra-grain ((buf buffer)
		   unit-rate
		   frames
		   gain
		   rate
		   start-pos
		   lp-freq
		   lp-q
		   lp-dist
		   peak-freq
		   peak-q
		   peak-gain
		   hp-freq
		   hp-q
		   a
		   length
		   r
		   spatial-pos)
  (foreach-channel
    (cout
     (pan2 
      (grain-core buf
		  unit-rate
		  frames
		  gain
		  rate
		  start-pos
		  lp-freq
		  lp-q
		  lp-dist
		  peak-freq
		  peak-q
		  peak-gain
		  hp-freq
		  hp-q
		  a
		  length
		  r)
      spatial-pos))))


