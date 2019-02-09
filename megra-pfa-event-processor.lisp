(in-package :megra)

;; graph-based event-generator, the main one ...
(defclass mpfa-event-processor (event-processor)
  ((source-mpfa :accessor source-mpfa :initarg :mpfa)
   (copy-events :accessor copy-events :initarg :copy-events :initform t)
   (combine-mode :accessor combine-mode :initarg :combine-mode)
   (combine-filter :accessor combine-filter :initarg :combine-filter)))

(defmethod current-events ((m mpfa-event-processor) &key)
  (list (current-events (source-mpfa m))))

(defmethod current-transition ((m mpfa-event-processor) &key)
  (list (current-transition (source-mpfa m))))

;; -------------------------------------------------------------- ;;
;; infer an mpfa event processor form a set of user-defined rules ;;
;; -------------------------------------------------------------- ;;
(defmacro events (&rest mappings)
  (let ((mapping (make-hash-table :test #'equal)))
    (loop for m in mappings 
       do (setf (gethash (car m) mapping) (eval (cadr m))))
    mapping))

(defmacro rules (&rest rules)
  (let ((duration-mapping (make-hash-table))
	(plain-rule-list (list)))
    (loop for rule in rules
       do (progn
	    (push (subseq rule 0 3) plain-rule-list)
	    ;; if the rule has a specific duration ... 
	    (when (nth 3 rule)
	      (setf (gethash (list (car (reverse (car rule))) (cadr rule))
			     duration-mapping)
		    (nth 3 rule))) ))
    `(list ,duration-mapping ',plain-rule-list)))

(defun infer (name events rules &key (dur 200))
  (let* ((new-mpfa (vom::infer-st-pfa-list (cadr rules)))
	 (new-proc (make-instance 'mpfa-event-processor :name name :mpfa new-mpfa))
	 (init-sym (car (alexandria::hash-table-keys events))))
    (change-class new-mpfa 'mpfa)
    (setf (mpfa-default-duration new-mpfa) dur)
    (setf (mpfa-event-dictionary new-mpfa) events)
    (setf (mpfa-transition-durations new-mpfa) (car rules))
    (setf (mpfa-last-symbol new-mpfa) init-sym)
    (vom::pfa-set-current-state new-mpfa (list init-sym))
    (setf (gethash name *processor-directory*) new-proc)))
