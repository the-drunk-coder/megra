(in-package :megra)

;; generic event-processor
(defclass event-processor ()
  ((pull-events)
   (pull-transition)       
   (successor :accessor successor :initform nil)
   (predecessor :accessor predecessor :initform nil)   
   (current-events)      ;; abstract
   (current-transition)  ;; abstract   
   (chain-bound :accessor chain-bound :initform nil)   
   (name :accessor name :initarg :name)
   (tempo-mod-stack :accessor tmods :initform nil)
   (combine-mode :accessor combine-mode :initarg :combine-mode :initform 'auto)
   (affect-transition :accessor affect-transition :initarg :affect-transition :initform nil)))

(defmethod pull-events ((e event-processor) &key (skip-successor nil))
  (if skip-successor
      (current-events e)
      (if (successor e)
          (apply-self e (pull-events (successor e)))
          (current-events e))))

(defmethod push-tmod ((e event-processor) tmod &key)
  (push tmod (tmods e)))

(defmethod pop-tmod ((e event-processor) &key)
  (pop (tmods e)))

;; events are the successor events 
(defmethod apply-self ((g event-processor) events &key)
  (combine-events (current-events g) events :mode (combine-mode g) :filter (combine-filter g)))

(defmethod apply-self-2 ((g event-processor) other-events events &key)
  (combine-events other-events events :mode (combine-mode g) :filter (combine-filter g)))

(defmethod apply-self-transition ((g event-processor) current-transition transition &key)
  (combine-events current-transition transition :mode (combine-mode g) :filter (combine-filter g)))

(defmethod pull-transition ((e event-processor) &key (skip-successor nil))
  (if skip-successor
      (current-transition e)
      (let ((cur-trans (current-transition e)))
        (when (tmods e)
          (setf (transition-duration (car cur-trans)) (* (transition-duration (car cur-trans)) (pop-tmod e))))
        (if (successor e)            
	    (if (affect-transition e)
	        (apply-self-transition e cur-trans (pull-transition (successor g)))
	        (pull-transition (successor e)))
            cur-trans))))

;; pass -- default 
(defmethod current-transition ((m event-processor) &key))
