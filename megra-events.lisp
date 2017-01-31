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
  ((pos :accessor pos :initarg :position :initform 0.5)))

(defclass tuned-instrument-event (pitch-event instrument-event level-event duration-event) ())

(defclass midi-event (tuned-instrument-event) ())

(defclass grain-event (level-event duration-event spatial-event)
  ((speed :initarg :speed :initform 1.0)
   (start :accessor start :initarg :start :initform 0.0)
   (hp-freq :accessor hp-freq :initarg :hp-freq :initform 10)
   (hp-q :accessor hp-q :initarg :hp-q :initform 1)
   (pf-freq :accessor peak-freq :initarg :peak-freq :initform 1000)
   (pf-q :accessor peak-q :initarg :peak-q :initform 10 )
   (pf-gain :accessor peak-gain :initarg :pf-gain :initform 0.0) 
   (lp-freq :accessor lp-freq :initarg :lp-freq :initform 19000)
   (lp-q :accessor lp-q :initarg :lp-q :initform 1.0)
   (lp-dist :accessor lp-dist :initarg :lp-dist :initform 0.0)
   (atk :accessor atk :initarg :atk)
   (len :accessor len :initarg :len)
   (rel :accessor rel :initarg :rel)   
   (sample-folder :accessor sample-folder)
   (sample-file :accessor sample-file)))



;; combining events ... simple for now ...
(defmethod combine-events (events-a events-b)
  (append events-a events-b))

(defclass transition ()
    ((duration :accessor transition-duration :initarg :dur)))



				        



