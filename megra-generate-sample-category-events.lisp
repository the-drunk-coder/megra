(in-package :megra)

(defun average-sample-length-ms (categ)
  (let ((accum 0.0)
	(count 0)
	file)
    (loop for path in
	 (cl-fad::list-directory (concatenate 'string cm::*sample-root*
					      (string-downcase categ)))
       do (cffi::with-foreign-object (sfinfo '(:struct cl-libsndfile::SF_INFO))
	    (setf file
		  (cl-libsndfile::sf_open (namestring path)
					  cl-libsndfile::SFM_READ sfinfo))
	    (let* ((frames (cffi::foreign-slot-value sfinfo
			    '(:struct cl-libsndfile::SF_INFO)
			    'cl-libsndfile::frames))
		   (sr (cffi::foreign-slot-value sfinfo
						 '(:struct cl-libsndfile::SF_INFO)
						 'cl-libsndfile::samplerate))
		   (length-in-ms (floor (* 1000.0 (/ frames sr)))))
	      ;;(print path)
	      (incf count)
	      (setf accum (+ accum length-in-ms)))
	    (cl-libsndfile::sf_close file)))
    (floor (/ accum count))))

(defun is-file (path)
  (pathname-name (probe-file path)))

;; create an event constructor for each sample category ... 
(loop for path in
     (cl-fad::list-directory cm::*sample-root*)
   when (not (is-file path))
   do (let ((dirname (car (last (pathname-directory path)))))
	(when (and (not (equal dirname "IR"))
		   (not (equal dirname ".git")))
	  (let* ((avg (average-sample-length-ms dirname))
		 (dur (cond ((< avg 64) 64)
			   ((> avg 1024) 1024)
			   (t (float (floor (/ avg 4)))))))
	    (eval `(define-category-sampling-event ,dirname ,dur))))))
