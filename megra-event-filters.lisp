(in-package :megra)

;; some default event filters
(defmethod all-p ((e event) &key) t)

(defmethod transition-p ((e event) &key)
  (typep e 'transition))

(defun snare-p (event)
  (or (member 'snare (event-tags event))
      (member 'sn (event-tags event))))

(defun bd-p (event)
  (or (member 'bassdrum (event-tags event))
      (member 'bd (event-tags event))))

(defun ride-p (event)
  (member 'ride (event-tags event)))

(defun hat-p (event)
  (member 'hat (event-tags event)))
