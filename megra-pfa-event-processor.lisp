(in-package :megra)

;; graph-based event-generator, the main one ...
(defclass mpfa-event-processor (event-processor)
  ((source-mpfa :accessor source-mpfa :initarg :mpfa)
   (copy-events :accessor copy-events :initarg :copy-events :initform t)
   (combine-mode :accessor combine-mode :initarg :combine-mode)
   (combine-filter :accessor combine-filter :initarg :combine-filter)))

(defmethod current-events ((m mpfa-event-processor) &key)
  (current-events (source-mpfa m)))

(defmethod current-transition ((m mpfa-event-processor) &key)
  (current-transition (source-mpfa m)))
