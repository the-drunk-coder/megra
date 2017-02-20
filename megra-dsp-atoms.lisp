(in-package :scratch)

;; tito's optimized version
(define-vug pan-ambi-3rd-sn3d-tito (in azi ele)
  "3rd order ambisonics encoder (no distance coding), ACN, SN3D"
  (with-samples ((ux (* (cos azi) (cos ele)))
                 (uy (* (sin azi) (cos ele)))
                 (uz (sin ele))
		 (ux2 (* ux ux))
		 (uy2 (* uy uy))
		 (uz2 (* uz uz)))
    (with ((chans (make-frame 16)))
      (declare (frame chans))
      (initialize
       (setf (smp-ref chans 0) (sample 1))
       (with-follow (ele)
         (setf (smp-ref chans 2) uz
               (smp-ref chans 6) (* 0.5 (- (* 3 uz2) 1))
               (smp-ref chans 12) (* 0.5 uz (- (* 5 uz2) 3))))
       (with-follow (azi ele)
         (setf (smp-ref chans 1) uy
               (smp-ref chans 3) ux
               (smp-ref chans 4) (* 1.73205080757 ux uy)
               (smp-ref chans 5) (* 1.73205080757 uy uz)
               (smp-ref chans 7) (* 1.73205080757 ux uz)
               (smp-ref chans 8) (* 0.86602540378 (- ux2 uy2))
               (smp-ref chans 9) (* 0.79056941504 uy (- (* 3 ux2) uy2))
               (smp-ref chans 10) (* 3.87298334621 uz ux uy)
               (smp-ref chans 11) (* 0.61237243569 uy (- (* 5 uz2) 1))
               (smp-ref chans 13) (* 0.61237243569 ux (- (* 5 uz2) 1))
               (smp-ref chans 14) (* 1.9364916731 uz (- ux2 uy2))
               (smp-ref chans 15) (* 0.79056941504 ux (- ux2 (* 3 uy2))))))
      (if (< current-channel 16)
          (* (smp-ref chans current-channel)
             (let ((current-channel 0)) in))
          +sample-zero+))))

;; my original version
(define-vug pan-ambi-3rd-sn3d (in azi ele)
  "3rd order ambisonics encoder (no distance coding), ACN, SN3D"
  (with-samples ((ux (* (cos azi) (cos ele)))
                 (uy (* (sin azi) (cos ele)))
                 (uz (sin ele))
		 (ux2 (* ux ux))
		 (uy2 (* uy uy))
		 (uz2 (* uz uz)))
    (cond ((= current-channel 0) (* 1 1 in))
          ((= current-channel 1) (* 1 uy in))
	  ((= current-channel 2) (* 1 uz in))
	  ((= current-channel 3) (* 1 ux in))
	  ((= current-channel 4) (* 1.73205080757 ux uy in))
	  ((= current-channel 5) (* 1.73205080757 uy uz in))
	  ((= current-channel 6) (* 0.5 (- (* 3 uz2) 1) in))
	  ((= current-channel 7) (* 1.73205080757 ux uz in))
	  ((= current-channel 8) (* 0.86602540378 (- ux2 uy2) in))
	  ((= current-channel 9) (* 0.79056941504 uy (- (* 3 ux2) uy2) in))
	  ((= current-channel 10) (* 3.87298334621  uz ux uy in))
	  ((= current-channel 11) (* 0.61237243569  uy (- (* 5 uz2) 1) in))
	  ((= current-channel 12) (* 0.5 uz (- (* 5 uz2) 3) in))				       
	  ((= current-channel 13) (* 0.61237243569 ux (- (* 5 uz2) 1) in))
	  ((= current-channel 14) (* 1.9364916731 uz (- ux2 uy2) in))
	  ((= current-channel 15) (* 0.79056941504 ux (- ux2 (* 3 uy2)) in))
	  (t +sample-zero+))))

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

(define-vug convorev (in (revbuf pvbuffer) rev gain a length r)
  (* (delay-s (envelope (make-local-envelope `(0 ,gain ,gain 0)
					     `(,a ,(+ length 2.5) ,r)) 1 1 #'free)
	      (pvbuffer-fft-size revbuf) (ash (pvbuffer-fft-size revbuf) -1))	   
     (+ (* (- 1 rev) (delay-s in (pvbuffer-fft-size revbuf) (ash (pvbuffer-fft-size revbuf) -1)))
	(* rev (part-convolve in revbuf)))))

