(in-package :megra)

;; controller stuff for akai lpd8
(defun register-knob (knob fun)
  (let ((resp (incudine::make-responder cm::*midiin*
					(lambda (st d1 d2)
					  (when (eql d1 knob)
					    (funcall fun d2))))))
    (when (gethash knob *midi-responders*)
      (incudine::remove-responder (gethash knob *midi-responders*)))
    (setf (gethash knob *midi-responders*) resp)))

(defun register-pad (pad fun &key (off nil) (toggle t))
  (let* ((pad-id (+ pad 35))
	 (resp (incudine::make-responder
		cm::*midiin*
		(lambda (st d1 d2)	   
		  (when (eql d1 pad-id)
		    ;; note on
		    (when (eql st 144)
		      (funcall fun d2))
		    ;; note off
		    (when (eql st 128)
		      (when off
			(funcall off d2))
		      ;; toggle light
		      (when toggle
			(if (gethash pad-id *pad-toggle-states*)
			    (setf (gethash pad-id *pad-toggle-states*) nil)
			    (progn			       
			      (setf (gethash pad-id *pad-toggle-states*) t)
			      (jackmidi:write cm::*midiout*
					      (coerce `(144 ,pad-id 96) 'jackmidi:data)))))))))))
    (when (gethash pad-id *midi-responders*)
      (incudine::remove-responder (gethash pad-id *midi-responders*)))
    (setf (gethash pad-id *midi-responders*) resp)))

(defun clear-midi-responders ()
  (labels ((rem-resp (key responder)
	     (incudine::remove-responder responder)))
    (maphash #'rem-resp *midi-responders*)))

(defun midi->range (midi-val range)
  (car (multiple-value-list (round (* range (/ midi-val 127))))))  
