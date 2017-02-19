(in-package :scratch)

(defvar *rev-crypta*
  (buffer->pvbuffer (buffer-load
		     (concatenate 'string "/home/nik/SAMPLES/" "IR/ir1_-_iringresso_new.wav"))
		    2048))

(defvar *rev-chapel*
  (buffer->pvbuffer (buffer-load
		     (concatenate 'string "/home/nik/SAMPLES/" "IR/ir1.wav"))
		    2048))

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
		   spatial-pos
		   (revbuf pvbuffer))
  (with-samples ((grain (grain-gen-free
			 buf
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
			 r)))
    (foreach-frame
      (foreach-channel
	(cout (pan2 (delay-s grain (pvbuffer-fft-size revbuf) (ash (pvbuffer-fft-size revbuf) -1)) spatial-pos))))))

(dsp! megra-grain-rev ((buf buffer)
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
		       spatial-pos
		       rev
		       (revbuf pvbuffer))
  (with-samples ((grain (grain-gen-id
			 buf 
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
			 r)))
    (foreach-frame
      (foreach-channel
	(cout
	 (pan2
	  (convorev grain revbuf rev gain a length r)
	  spatial-pos))))))

(dsp! megra-grain-ambi ((buf buffer)
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
		   azi
		   ele
		   (revbuf pvbuffer))
  (with-samples ((grain (grain-gen-free
			 buf
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
			 r)))
    (foreach-frame
      (foreach-channel
	(cout (pan-ambi-3rd-sn3d (delay-s grain (pvbuffer-fft-size revbuf) (ash (pvbuffer-fft-size revbuf) -1)) azi ele))))))

(dsp! megra-grain-ambi-rev ((buf buffer)
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
		       azi
		       ele	    
		       rev
		       (revbuf pvbuffer))
  (with-samples ((grain (grain-gen-id
			 buf 
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
			 r)))
    (foreach-frame
      (foreach-channel
	(cout
	 (pan-ambi-3rd-sn3d
	  (convorev grain revbuf rev gain a length r)
	  azi ele))))))




