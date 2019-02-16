(in-package :megra)

;; graph-based event-generator, the main one ...
(defclass mpfa-event-processor (event-processor)
  ((source-mpfa :accessor source-mpfa :initarg :mpfa)
   (copy-events :accessor copy-events :initarg :copy-events :initform t)
   (combine-filter :accessor combine-filter :initarg :combine-filter :initform #'all-p)))

(defmethod current-events ((m mpfa-event-processor) &key)
  (if (copy-events m)
      (deepcopy (current-events (source-mpfa m)))
      (current-events (source-mpfa m))))

(defmethod current-transition ((m mpfa-event-processor) &key)
  (list (current-transition (source-mpfa m))))

(defun sstring (string-as-sym)
  "convenience method to enter sample strings without spacesx"
  (let ((sname (if (typep string-as-sym 'string)
		   string-as-sym
		   (symbol-name string-as-sym))))
    (loop for c in (coerce sname 'list)
       collecting (intern (string-upcase (string c))))))

;; data transformation macro to define event lists more easily
(defmacro events (&rest mappings)
  (let ((mapping (make-hash-table :test #'equal)))
    (loop for m in mappings 
       do (setf (gethash (car m) mapping) (mapcar #'eval (cdr m))))
    mapping))

;; data transformation macro to define transition rules  more easily
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

;; -------------------------------------------------------------- ;;
;; infer an mpfa event processor form a set of user-defined rules ;;
;; -------------------------------------------------------------- ;;
(defun infer (name events rules &key (dur 200))
  (let* ((new-mpfa (vom::infer-st-pfa-list (cadr rules)))
	 (old-proc (gethash name *processor-directory*))
	 (new-proc
	  (if (and old-proc (typep old-proc 'mpfa-event-processor))
	      old-proc
	      (make-instance 'mpfa-event-processor :name name :mpfa new-mpfa)))
	 (init-sym (car (alexandria::hash-table-keys events))))
    (change-class new-mpfa 'mpfa)
    (if (and old-proc (typep old-proc 'mpfa-event-processor))
	(setf (source-mpfa old-proc) new-mpfa))
    (setf (mpfa-default-duration new-mpfa) dur)
    (setf (mpfa-event-dictionary new-mpfa) events)
    (setf (mpfa-transition-durations new-mpfa) (car rules))
    (setf (mpfa-last-symbol new-mpfa) init-sym)
    (vom::pfa-set-current-state new-mpfa (list init-sym))
    (setf (gethash name *processor-directory*) new-proc)))

;; ---------------------------------------------------- ;;
;; learn an mpfa event processor form a sample sequence ;;
;; ---------------------------------------------------- ;;
(defun learn (name events sample-string &key (dur 200)
					  (bound 3)
					  (epsilon 0.001)
					  (size 50))
  (let* ((alphabet (reverse (alexandria::hash-table-keys events)))
	 (new-mpfa (vom::learn-pfa alphabet bound epsilon size sample-string))
	 (old-proc (gethash name *processor-directory*))
	 (new-proc
	  (if (and old-proc (typep old-proc 'mpfa-event-processor))
	      old-proc
	      (make-instance 'mpfa-event-processor :name name :mpfa new-mpfa)))
	 (init-sym (car alphabet)))        
    (vom::pfa-set-current-state new-mpfa (list init-sym))
    (change-class new-mpfa 'mpfa)
    (if (and old-proc (typep old-proc 'mpfa-event-processor))
	(setf (source-mpfa old-proc) new-mpfa))
    (setf (mpfa-default-duration new-mpfa) dur)
    (setf (mpfa-event-dictionary new-mpfa) events)
    (setf (mpfa-last-symbol new-mpfa) init-sym)        
    (setf (gethash name *processor-directory*) new-proc)))

