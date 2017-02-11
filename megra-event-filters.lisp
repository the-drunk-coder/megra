;; some default event filters
(defmethod all-p ((e event) &key)
  t)

(defmethod transition-p ((e event) &key)
  (typep e 'transition))
