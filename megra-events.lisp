;; the atomic units of music - event and transition ...
(defclass event ()
    ((source :accessor event-source)))

(defclass string-event (event)
    ((msg :accessor msg :initarg :msg)))

(defclass pitch-event (event)
  ((pitch :accessor pitch :initarg :pitch)))

(defclass level-event (event)
  ((lvl :accessor lvl :initarg :level)))

(defclass duration-event (event)
  ((dur :accessor dur :initarg :duration)))

(defclass instrument-event (event)
  ((inst :accessor inst :initarg :instrument)))

(defclass spatial-event (event)
  ((pos :accessor pos :initarg :position)))

(defclass tuned-instrument-event (pitch-event instrument-event level-event duration-event) ())

(defclass midi-event (tuned-instrument-event) ())


;; combining events ... simple for now ...
(defmethod combine-events (events-a events-b)
  (append events-a events-b))

(defclass transition ()
    ((duration :accessor transition-duration :initarg :dur)))



				        



