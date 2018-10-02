(in-package :megra)

;; some default event filters
(defmethod all-p ((e event) &key) t)

(defun snare-p (event)
  (or (member 'snare (event-tags event))
      (member 'sn (event-tags event))))

(defun ride-p (event)
  (member 'ride (event-tags event)))

(defun hat-p (event)
  (member 'hat (event-tags event)))
