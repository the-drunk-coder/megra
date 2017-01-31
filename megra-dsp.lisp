(defparameter *buffer-directory* (make-hash-table :test 'equal))
(defparameter *sample-root* "/home/nik/SAMPLES/" )

(defstruct buffer-data 
  buffer
  buffer-rate
  buffer-frames)

(make-buffer-data )

(in-package :scratch)

;; unit rate 

(dsp! megra-grain ((buf buffer)
		   unit-rate
		   frames
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
		   spatial-pos
		   interpolation)
  (foreach-channel
    (cout
     (pan2 (* 
	    (lpf18
	     (peak-eq 
	      (hpf 	
	       (* (envelope (make-local-envelope '(0 1 1 0) `(,a ,length ,r) ) 1 1 #'free)
		  (buffer-read buf (* (phasor (* rate unit-rate) start-pos) frames)
			       :wrap-p nil :interpolation interpolation))
	       hp-freq hp-q)
	      peak-freq peak-q peak-gain)
	     lp-freq lp-q lp-dist))
	   spatial-pos))))
