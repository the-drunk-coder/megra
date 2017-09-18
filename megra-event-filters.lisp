(in-package :megra)

;; some default event filters
(defmethod all-p ((e event) &key) t)

(defmethod transition-p ((e event) &key)
  (typep e 'transition))

(defun snare-p (event)
  (or (member 'snare (event-tags event))
      (member 'sn(event-tags event))))
