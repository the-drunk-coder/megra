(in-package :scratch)

(compile-vug 'envelope 'sample)
(compile-vug 'lpf18 'sample)
(compile-vug 'hpf 'sample)
(compile-vug 'peak-eq 'sample)
(compile-vug 'phasor 'sample)
(compile-vug 'delay-s 'sample)

(define-vug grain-gen-free ((buf buffer)
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
  (with-samples ((snippet (buffer-read buf (* (phasor (* rate unit-rate) start-pos) frames)
				       :wrap-p nil :interpolation :cubic) ))
    (lpf18
     (peak-eq 
      (hpf 	
       (* (envelope (make-local-envelope `(0 ,gain ,gain 0) `(,a ,length ,r)) 1 1 #'free)
	  snippet )
       hp-freq hp-q)
      peak-freq peak-q peak-gain)
     lp-freq lp-q lp-dist)))

(define-vug grain-gen-id ((buf buffer)
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
  (with-samples ((snippet (buffer-read buf (* (phasor (* rate unit-rate) start-pos) frames)
				       :wrap-p nil :interpolation :cubic) ))
    (lpf18
     (peak-eq 
      (hpf 	
       (* (envelope (make-local-envelope `(0 ,gain ,gain 0) `(,a ,length ,r)) 1 1 #'identity)
	  snippet )
       hp-freq hp-q)
      peak-freq peak-q peak-gain)
     lp-freq lp-q lp-dist)))


;; compile
(compile-vug 'grain-gen-free 'sample)
(compile-vug 'grain-gen-id 'sample)

(define-vug convorev (in (revbuf pvbuffer) rev gain a length r)
  (* (delay-s (envelope (make-local-envelope `(0 ,gain ,gain 0)
					     `(,a ,(+ length 2.5) ,r)) 1 1 #'free)
	      (pvbuffer-fft-size revbuf) (ash (pvbuffer-fft-size revbuf) -1))	   
     (+ (* (- 1 rev) (delay-s in (pvbuffer-fft-size revbuf) (ash (pvbuffer-fft-size revbuf) -1)))
	(* rev (part-convolve in revbuf)))))

;;compile
(compile-vug 'convorev 'sample)


