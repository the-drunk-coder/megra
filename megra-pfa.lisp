;; megra-flavoured PFAs

;; needs the generic variable-order markov chain library ...
(in-package :megra)

(defun gen-transition-durations (sample-string-with-durations dict)
  (loop for (a b) on sample-string-with-durations while b
     do (setf (gethash (list (car a) (car b)) dict) (cadr a))))

(defun gen-event-dictionary (alphabet-with-payload dict)
  (loop for entry in alphabet-with-payload
       do (setf (gethash (car entry) dict) (cdr entry))))

(defclass mpfa (vom::st-pfa)
  ((transition-durations :accessor mpfa-transition-durations
			 :initform (make-hash-table :test #'equal))
   (event-dictionary :accessor mpfa-event-dictionary
		     :initform (make-hash-table :test #'equal))
   (last-symbol :accessor mpfa-last-symbol)
   (default-duration :accessor mpfa-default-duration)))

(defmethod current-events ((m mpfa) &key)
  (gethash (mpfa-last-symbol m) (mpfa-event-dictionary m)))

(defmethod current-transition ((m mpfa) &key)
  (let* ((next-symbol (vom::pfa-next-symbol m))
	 (dur (gethash (list (mpfa-last-symbol m)
			     (setf (mpfa-last-symbol m) next-symbol))
		       (mpfa-transition-durations m))))
    (make-instance 'transition-event
		   :dur (if dur dur (mpfa-default-duration m))
		   :tags '(transition))))

;; sample string needs to have unique transition durations
(defun learn-mpfa (alphabet-with-payload
		   bound
		   epsilon
		   n
		   sample-string-with-durations
		   default-duration)
  "learn an mpfa from an alphabet and a number od samples ..."
  (let* ((alphabet (mapcar #'car alphabet-with-payload))
	 (sample-string (mapcar #'car sample-string-with-durations))
	 (new-pfa (vom::learn-pfa alphabet bound epsilon n sample-string)))
    ;; dirty, but hey ... 
    (change-class new-pfa 'mpfa)
    (setf (mpfa-default-duration new-pfa) default-duration)
    (gen-transition-durations sample-string-with-durations
			      (mpfa-transition-durations new-pfa))
    (gen-event-dictionary alphabet-with-payload
			  (mpfa-event-dictionary new-pfa))
    new-pfa))


