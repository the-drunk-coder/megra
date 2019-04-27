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
  "convenience method to enter sample strings without spaces"
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

(defmacro p-events (event-plist)
  `(funcall (lambda ()
	      (let ((mapping (make-hash-table :test #'equal))
		    (key))    
		(loop for m in ',event-plist 
		   do (if (or (typep m 'symbol) (typep m 'number))
 			  (progn
			    (setf key m)
			    (setf (gethash key mapping) (list)))
			  (let ((me (eval m))
				(le (gethash key mapping)))			    
			    (setf (gethash key mapping) (nconc le (list me))))))
		mapping))))

;; data transformation macro to define transition rules more easily
(defmacro rules (&rest rules)
  (let ((duration-mapping (make-hash-table :test #'equal))
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

(defun rules-list (rules)
  (let ((duration-mapping (make-hash-table :test #'equal))
	(plain-rule-list (list)))
    (loop for rule in rules
       do (progn
	    (push (subseq rule 0 3) plain-rule-list)
	    ;; if the rule has a specific duration ... 
	    (when (nth 3 rule)
	      (setf (gethash (list (car (reverse (car rule))) (cadr rule))
			     duration-mapping)
		    (nth 3 rule))) ))
    (list duration-mapping plain-rule-list)))

;; -------------------------------------------------------------- ;;
;; infer an mpfa event processor form a set of user-defined rules ;;
;; -------------------------------------------------------------- ;;
(defun infer (name events rules &key (dur *global-default-duration*))
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
(defun learn (name events sample-string &key (dur *global-default-duration*)
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
    (vom::pfa->st-pfa new-mpfa)
    (vom::pfa-set-current-state new-mpfa (list init-sym))    
    (change-class new-mpfa 'mpfa)
    (if (and old-proc (typep old-proc 'mpfa-event-processor))
	(setf (source-mpfa old-proc) new-mpfa))
    (setf (mpfa-default-duration new-mpfa) dur)
    (setf (mpfa-event-dictionary new-mpfa) events)
    (setf (mpfa-last-symbol new-mpfa) init-sym)        
    (setf (gethash name *processor-directory*) new-proc)))


(defmacro slearn (name events sample-string &key (dur *global-default-duration*)
					      (bound 3)
					      (epsilon 0.001)
					      (size 50))
  `(funcall (lambda ()
	      (learn ,name (p-events ,events) (sstring ,sample-string)
		     :dur ,dur
		     :bound ,bound
		     :epsilon ,epsilon
		     :size ,size))))

;; abstractions ... 
(defmacro nuc2 (name event &key (dur *global-default-duration*))
  `(funcall #'(lambda ()
                (infer ,name
	               (events (1 ,event))
	               (rules ((1) 1 1.0))
	               :dur ,dur))))

(defun grow2 (name &key (var 0.1) (hist 2) (ord 2) (exit 1) method durs funct)
  (let* ((proc (gethash name *processor-directory*))
	 ;; this one needs to be adapted to keep the old methods
	 ;; alive ! 
	 (growth-result (vom::grow-st-pfa (source-mpfa proc) hist ord exit))
	 (template (gethash (first growth-result)
			    (mpfa-event-dictionary (source-mpfa proc)))))
    (setf (gethash (second growth-result)
		   (mpfa-event-dictionary (source-mpfa proc)))
	  (deepcopy template :imprecision var :functors funct))
    ;; default duration will be picked for now ...
    ))

(defun cyc2 (name events  &key (dur *global-default-duration*) (rep 0) (max-rep 4))
  (let ((count 1)
	(rules (list))
	(event-mapping (make-hash-table :test #'equal))
	(real-events (if (typep events 'string)
			 (string->cycle-list events)
			 events)))
    (loop for (a b) on real-events while b
          do (cond
	       ((and (or (typep a 'event) (typep a 'list)) (or (typep b 'event) (typep b 'list)))
	        (setf (gethash count event-mapping) (if (typep a 'list) a (list a)))
	        (setf (gethash (+ count 1) event-mapping) (if (typep b 'list) b (list b)))
	        (let ((new-rule (list (list count) (incf count) 1.0)))
	          (setf rules (nconc rules (list new-rule)))))
	       ((and (or (typep a 'event) (typep a 'list)) (typep b 'number))
	        (setf (gethash count event-mapping) (if (typep a 'list) a (list a))))
	       ((and (typep a 'number) (or (typep b 'event) (typep b 'list)))
	        (setf (gethash (+ count 1) event-mapping) (if (typep b 'list) b (list b)))
	        (let ((new-rule (list (list count) (incf count) 1.0 a)))
	          (setf rules (nconc rules (list new-rule)))))))
    (if (typep (car (last real-events)) 'number)
	(setf rules (nconc rules (list (list (list count) 1 1.0 (car (last real-events))))))
	(setf rules (nconc rules (list (list (list count) 1 1.0)))))
    (infer name
	   event-mapping
	   (rules-list rules)
	   :dur dur)))

(defun mpfa->svg (name)
  (vom::pfa->svg (source-mpfa (gethash name *processor-directory*)) (symbol-name name) ))

