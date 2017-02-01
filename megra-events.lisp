;; the atomic units of music - event and transition ...
(defclass event ()
    ((source :accessor event-source)))

(defclass string-event (event)
    ((msg :accessor msg :initarg :msg)))

(defclass pitch-event (event)
  ((pitch :accessor pitch :initarg :pitch)))

(defclass level-event (event)
  ((lvl :accessor lvl :initarg :lvl)))

(defclass duration-event (event)
  ((dur :accessor dur :initarg :dur)))

(defclass instrument-event (event)
  ((inst :accessor inst :initarg :inst)))

(defclass spatial-event (event)
  ((pos :accessor pos :initarg :pos)))

(defclass tuned-instrument-event (pitch-event instrument-event level-event duration-event) ())

(defclass midi-event (tuned-instrument-event) ())

(defclass grain-event (level-event duration-event spatial-event)
  ((rate :accessor rate :initarg :rate)
   (start :accessor start :initarg :start)
   (hp-freq :accessor hp-freq :initarg :hp-freq)
   (hp-q :accessor hp-q :initarg :hp-q)
   (pf-freq :accessor pf-freq :initarg :pf-freq)
   (pf-q :accessor pf-q :initarg :pf-q)
   (pf-gain :accessor pf-gain :initarg :pf-gain) 
   (lp-freq :accessor lp-freq :initarg :lp-freq)
   (lp-q :accessor lp-q :initarg :lp-q)
   (lp-dist :accessor lp-dist :initarg :lp-dist)
   (atk :accessor atk :initarg :atk)
   (rel :accessor rel :initarg :rel)   
   (sample-folder :accessor sample-folder :initarg :sample-folder)
   (sample-file :accessor sample-file :initarg :sample-file)
   (sample-location :accessor sample-location)))

(defmethod initialize-instance :after ((g grain-event) &key)
  (setf (sample-location g) (concatenate 'string *sample-root* (sample-folder g) "/" (sample-file g) ".wav")))

;; combining events ... simple for now ...
(defmethod combine-events (events-a events-b)
  (append events-a events-b))

(defclass transition ()
    ((duration :accessor transition-duration :initarg :dur)))



				        



