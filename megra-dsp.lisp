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
    (foreach-channel
      (cout
       (pan2
	(convorev grain revbuf rev gain a length r)
	spatial-pos)))))

  
;;(dsp! da-buzz ()
;;  (foreach-channel
;;      (cout
;;       (pan2
;;	(* (envelope (make-local-envelope `(0 1 0.5 0.5 0)
;;						   `(0.005 0.03 0.6 0.1)) 1 1 #'free)
;;	   (gbuzz 100 .5 50 1 .3)) 0.9))))

;;(da-buzz)

;;(free 0)

;;; There are two keywords in BUFFER->PVBUFFER, START and FRAMES,
;;; respectively the offset and the number of frames of the input
;;; buffer. The default is to use the whole buffer.

;;(dsp! pconv-test ((inbuf buffer) (pvbuf pvbuffer) dry wet)
;; (with-samples ((in (buffer-play inbuf 1 0 t #'free)))
;;   (stereo (* dry (delay-s in 65536
;;                          (ash (pvbuffer-fft-size pvbuf) -1))))
;;  (foreach-channel (cout (* wet (part-convolve in pvbuf))))))



