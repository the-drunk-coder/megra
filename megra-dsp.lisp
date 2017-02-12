(in-package :scratch)

(defvar *rev-crypta*
  (buffer->pvbuffer (buffer-load
		     (concatenate 'string "/home/nik/SAMPLES/" "IR/ir1_-_iringresso_new.wav"))
		    8192))

(defvar *rev-chapel*
  (buffer->pvbuffer (buffer-load
		     (concatenate 'string "/home/nik/SAMPLES/" "IR/ir1.wav"))
		    8192))


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
  (with-samples ((grain (lpf18
			 (peak-eq 
			  (hpf 	
			   (* (envelope (make-local-envelope
					 `(0 ,gain ,gain 0) `(,a ,length ,r) ) 1 1 #'free)
			      (buffer-read buf (* (phasor (* rate unit-rate) start-pos) frames)
					   :wrap-p nil :interpolation :cubic))
			   hp-freq hp-q)
			  peak-freq peak-q peak-gain)
			 lp-freq lp-q lp-dist)))
    (foreach-channel
      (cout (pan2 (delay-s grain 65536 (ash (pvbuffer-fft-size revbuf) -1)) spatial-pos)))))

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
  (with-samples ((grain (lpf18
			 (peak-eq 
			  (hpf 	
			   (* (envelope (make-local-envelope
					 `(0 ,gain ,gain 0) `(,a ,length ,r) ) 1 1 #'identity)
			      (buffer-read buf (* (phasor (* rate unit-rate) start-pos) frames)
					   :wrap-p nil :interpolation :cubic))
			   hp-freq hp-q)
			  peak-freq peak-q peak-gain)
			 lp-freq lp-q lp-dist)))
    (foreach-channel
      (cout
       (pan2
	(* (delay-s (envelope (make-local-envelope `(0 ,gain ,gain 0)
						   `(,a ,(+ length 2.5) ,r)) 1 1 #'free)
		    65536 (ash (pvbuffer-fft-size revbuf) -1))	   
	   (+ (* (- 1 rev) (delay-s grain 65536 (ash (pvbuffer-fft-size revbuf) -1)))
	      (* rev (part-convolve grain revbuf))))
	spatial-pos)))))

  



;;; There are two keywords in BUFFER->PVBUFFER, START and FRAMES,
;;; respectively the offset and the number of frames of the input
;;; buffer. The default is to use the whole buffer.

;;(dsp! pconv-test ((inbuf buffer) (pvbuf pvbuffer) dry wet)
;; (with-samples ((in (buffer-play inbuf 1 0 t #'free)))
;;   (stereo (* dry (delay-s in 65536
;;                          (ash (pvbuffer-fft-size pvbuf) -1))))
;;  (foreach-channel (cout (* wet (part-convolve in pvbuf))))))



