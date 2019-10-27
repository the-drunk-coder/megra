(in-package :megra)

(defun register-sample (sample-loc)
  ;; register sample ...
  (osc:message cm::*oscout* "/b_allocRead" "isii" *sc-sample-bufnums* sample-loc 0 -1)
  (setf (gethash sample-loc *sc-buffer-directory*) *sc-sample-bufnums*)
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
  (osc:message cm::*oscout* "/b_allocRead" "isii" 0 (concatenate 'string cm::*sample-root* "IR/" cm::*reverb-ir* "." cm::*sample-type*) 0 -1)
  (sleep 0.5)
  ;; allocate buffer 
  (osc:message cm::*oscout* "/b_alloc" "iiii" *ir-spectral-bufnum* *chapel-buflength* 1 0)
  (sleep 0.5)
  ;; prepare part conv
  (osc:message cm::*oscout* "/b_gen" "isii" *ir-spectral-bufnum* "PreparePartConv" 0 *chapel-ir-length*)
  (sleep 0.5))

(defun init-4ch-rev ()
    (osc:message cm::*oscout*   
	       "/s_new"	    
	       "siii"
	       "reverb_4ch" -1 0 1))

(defun init-8ch-rev ()
    (osc:message cm::*oscout*   
	       "/s_new"	    
	       "siii"
	       "reverb_8ch" -1 0 1))

(defun init-16ch-rev ()
    (osc:message cm::*oscout*   
	       "/s_new"	    
	       "siii"
	       "reverb_16ch" -1 0 1))

(defun init-24ch-rev ()
    (osc:message cm::*oscout*   
	       "/s_new"	    
	       "siii"
	       "reverb_24ch" -1 0 1))

(defun free-sample (bufnum)
  (osc:message cm::*oscout* "/b_free" "i" bufnum))

(defun free-all-samples ()
  ;; this is so amazing i just have to use it ...
  (loop for key being the hash-keys of *sc-buffer-directory*
     do (free-sample key))
  ;; clear client-side buffer
  (setf *sc-buffer-directory* (make-hash-table :test 'eql))
  (setf *sc-sample-bufnums* 2))



