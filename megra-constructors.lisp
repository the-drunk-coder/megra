(defun string-event (msg)
  (make-instance 'string-event :msg msg))

(defun node (id &rest content)
  (make-instance 'node :id id :content content))

(defun edge (src dest &key prob dur)
  (make-instance 'edge :src src :dest dest :prob prob :content `(,(make-instance 'transition :dur dur))))
