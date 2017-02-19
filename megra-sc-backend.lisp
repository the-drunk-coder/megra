(in-package :megra)

(defun register-sample (sample-loc)
  ;; register sample ...
  (output (new cm::osc 
	    :path "/b_allocRead"
	    :time (now)
	    :types "is"
	    :message `(,*sc-sample-bufnums* ,sample-loc)))
  (setf (gethash sample-loc *buffer-directory*) *sc-sample-bufnums*)
  (setf *sc-sample-bufnums* (1+ *sc-sample-bufnums*)))

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
  (setf *sc-sample-bufnums* 0))


