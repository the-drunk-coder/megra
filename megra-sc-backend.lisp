(in-package :megra)

(defun register-sample (sample-loc)
  ;; register sample ...
  (output (new cm::osc 
	    :path "/b_allocRead"
	    :time (now)
	    :types "isii"
	    :message `(,*sc-sample-bufnums* ,sample-loc 0 -1)))
  (setf (gethash sample-loc *buffer-directory*) *sc-sample-bufnums*)
  (setf *sc-sample-bufnums* (1+ *sc-sample-bufnums*)))

;; the buflength was empirically determined using sclang.
;; once this has been done, and the ir length doesn't change,
;; there's no need to use sclang anymore ... 
(defparameter *chapel-ir-length* 2048)
(defparameter *chapel-buflength* 63488)
(defparameter *ir-spectral-bufnum* 1)

;; this sequence of messages 
(defun prepare-ir ()
  ;; allocate and read buffer
  (output (new cm::osc 
	    :path "/b_allocRead"
	    :time (now)
	    :types "isii"
	    :message `(0 "/home/nik/SAMPLES/IR/ir1.wav" 0 -1)))
  (sleep 0.5)
  ;; allocate buffer 
  (output (new cm::osc 
	    :path "/b_alloc"
	    :time (now)
	    :types "iiii"
	    :message `(,*ir-spectral-bufnum* ,*chapel-buflength* 1 0)))
  (sleep 0.5)
  ;; prepare part conv
  (output (new cm::osc 
	    :path "/b_gen"
	    :time (now)
	    :types "isii"
	    :message `(,*ir-spectral-bufnum* "PreparePartConv" 0 ,*chapel-ir-length*))))

(defun free-sample (bufnum)
  (output (new cm::osc 
	    :path "/b_free"
	    :time (now)
	    :types "i"
	    :message `(,bufnum))))

(defun free-all-samples ()
  ;; this is so amazing i just have to use it ...
  (loop for key being the hash-keys of *buffer-directory*
     do (free-sample key))
  ;; clear client-side buffer
  (setf *buffer-directory* (make-hash-table :test 'eql))
  (setf *sc-sample-bufnums* 2))



