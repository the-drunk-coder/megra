					;the atomic units of music
(defclass event ()
    ((source :accessor event-source)))

(defclass string-event (event)
    ((message :accessor event-message :initarg :msg)))

(defclass pitch-event (event)
  ((pitch :accessor event-pitch :initarg :pitch)))

(defclass level-event (event)
  ((level :accessor event-level :initarg :level)))

(defclass duration-event (event)
  ((duration :accessor event-duration :initarg :duration)))

(defclass instrument-event (event)
  ((instrument :accessor event-instrument :initarg :instrument)))

(defclass spatial-event (event)
  ((position :accessor event-position :initarg :position)))

(defclass tuned-instrument-event (pitch-event instrument-event level-event duration-event) ())

(defclass midi-event (tuned-instrument-event) ())


					;simple for now ...
(defmethod combine-events (events-a events-b)
  (append events-a events-b))

(defclass transition ()
    ((duration :accessor transition-duration :initarg :dur)))



				        



