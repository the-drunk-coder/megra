(in-package :megra)

(defun register-sample (sample-loc)
  ;; register sample ...
  (osc:message cm::*oscout* "/b_allocRead" "isii" *sc-sample-bufnums* sample-loc 0 -1)
  (setf (gethash sample-loc *buffer-directory*) *sc-sample-bufnums*)
  (setf *sc-sample-bufnums* (1+ *sc-sample-bufnums*)))

;; the buflength was empirically determined using sclang.
;; once this has been done, and the ir length doesn't change,
;; there's no need to use sclang anymore ... 
(defparameter *chapel-ir-length* 2048)
(defparameter *chapel-buflength* 63488)
(defparameter *ir-spectral-bufnum* 1)

(defun init-sc-base-group ()
  (osc:message cm::*oscout* "/g_new" "iii" 1 0 0))

;; this sequence of messages 
(defun prepare-ir ()
  ;; allocate and read buffer
  (osc:message cm::*oscout* "/b_allocRead" "isii" 0 "/home/nik/SAMPLES/IR/ir1.wav" 0 -1)
  (sleep 0.5)
  ;; allocate buffer 
  (osc:message cm::*oscout* "/b_alloc" "iiii" *ir-spectral-bufnum* *chapel-buflength* 1 0)
  (sleep 0.5)
  ;; prepare part conv
  (osc:message cm::*oscout* "/b_gen" "isii"
	       *ir-spectral-bufnum* "PreparePartConv" 0 *chapel-ir-length*))

(defun free-sample (bufnum)
  (osc:message "/b_free" "i" bufnum))

(defun free-all-samples ()
  ;; this is so amazing i just have to use it ...
  (loop for key being the hash-keys of *buffer-directory*
     do (free-sample key))
  ;; clear client-side buffer
  (setf *buffer-directory* (make-hash-table :test 'eql))
  (setf *sc-sample-bufnums* 2))



