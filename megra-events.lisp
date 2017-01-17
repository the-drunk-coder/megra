					; the atomic units of music
(defclass event ()
    ((source :accessor event-source)))

(defclass string-event (event)
    ((message :accessor event-message :initarg :msg)))

					;simple for now ...
(defmethod combine-events (events-a events-b)
  (append events-a events-b))

(defclass transition ()
    ((duration :accessor transition-duration :initarg :dur)))



					; things to come:
					; pitch event

					; instrument event

					; dynamics event

					; duration event 

					; silent event

					; spatial event




