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

;;(defparameter *sn3d-norm-factors*
;;  `(1 ;;0
;;    1 ;;1
;;    1 ;;2
;;    1 ;;3
;;    ,(sqrt 3) ;;4
;;    ,(sqrt 3) ;;5
;;    0.5 ;;6
;;    ,(sqrt 3) ;;7
;;    ,(/ (sqrt 3) 2) ;;8
;;    ,(sqrt (/ 5 8)) ;;9
;;    ,(sqrt 15) ;;10
;;    ,(sqrt (/ 3 8)) ;;11
;;    0.5 ;;12
;;    ,(sqrt (/ 3 8)) ;;13
;;    ,(/ (sqrt 15) 2) ;;14
;;    ,(sqrt (/ 5 8)) ;;15
;;    ))


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
	  ((= current-channel 4) (* 1.7320508 ux uy in))
	  ((= current-channel 5) (* 1.7320508 uy uz in))
	  ((= current-channel 6) (* 0.5 (- (* 2 uz2) ux2 uy2) in))
	  ((= current-channel 7) (* 1.7320508 ux uz in))
	  ((= current-channel 8) (* 0.8660254 (- ux2 uy2) in))
	  ((= current-channel 9) (* 0.7905694 uy (- (* 3 ux2) uy2) in))
	  ((= current-channel 10) (* 3.8729835  uz ux uy in))
	  ((= current-channel 11) (* 0.61237246 uy (- (* 4 uz2) ux2 uy2) in))
	  ((= current-channel 12) (* 0.5 uz (- (* 2 uz2) (* 3 ux2) (* 3 uy2)) in))
	  ((= current-channel 13) (* 0.61237246  ux (- (* 4 uz2) ux2 uy2) in))
	  ((= current-channel 14) (* 1.9364917 uz (- ux2 uy2) in))
	  ((= current-channel 15) (* 0.7905694 ux (- ux2 (* 3 uy2)) in))
	  (t +sample-zero+))))

(compile-vug 'pan-ambi-3rd-sn3d 'sample)
