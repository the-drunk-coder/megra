(in-package :megra)

(defun string->cycle-list (str)
  "parse a cycle from a string to a list of events and durations"
  (let* ((split
           (cl-ppcre:split "\\s+"
           (cl-ppcre:regex-replace-all "\\]"
           (cl-ppcre:regex-replace-all "\\["
           (cl-ppcre:regex-replace-all "\\~" str "silence") "( ") " )")))
         (cycle (list))
         (stack (list))
         (stack-mode nil))
    (loop for token in split 
          do (cond ((string= token "(")
		    (setf stack-mode t))
		   ((string= token ")")
		    (setf stack-mode nil)
		    (setf cycle (nconc cycle (list stack)))
		    (setf stack (list)))
		   ((ignore-errors (parse-integer token)) (setf cycle (nconc cycle (list (parse-integer token)))))
		   (t (if stack-mode
		          (setf stack (nconc stack (list (let ((f-par (cl-ppcre:split ":" token)))  
							   (eval (read-from-string (format nil "(~{~a~^ ~})" f-par)))))))
		          (setf cycle (nconc cycle (list (let ((f-par (cl-ppcre:split ":" token)))  
							   (eval (read-from-string (format nil "(~{~a~^ ~})" f-par)))))))))))
    cycle))

(defun parse-cycle (events &key (dur *global-default-duration*) (rep 0) (max-rep 4))
  "parse a list of events and durations to a list of rules and event mappings"
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
                (if (> rep 0)
		    (if (< (random 100) rep)
                        (progn
                          (let ((new-rule (list (list count) count 0.5)))
	                    (alexandria::nconcf rules (list new-rule)))
                          (let ((new-rule (list (list count) (incf count) 0.5)))
	                    (alexandria::nconcf rules (list new-rule)))
		          (when max-rep
                            (let ((new-rule (list (make-list max-rep :initial-element (- count 1)) count 1.0)))
	                      (alexandria::nconcf rules (list new-rule)))))
                        (let ((new-rule (list (list count) (incf count) 1.0)))
	                  (alexandria::nconcf rules (list new-rule))))
                    (let ((new-rule (list (list count) (incf count) 1.0)))
	              (alexandria::nconcf rules (list new-rule)))))
	       ((and (or (typep a 'event) (typep a 'list)) (typep b 'number))
	        (setf (gethash count event-mapping) (if (typep a 'list) a (list a))))
	       ((and (typep a 'number) (or (typep b 'event) (typep b 'list)))
	        (setf (gethash (+ count 1) event-mapping) (if (typep b 'list) b (list b)))
                (if (> rep 0)
		    (if (< (random 100) rep)
                        (progn
                          (let ((new-rule (list (list count) count 0.5)))
	                    (alexandria::nconcf rules (list new-rule)))
                          (let ((new-rule (list (list count) (incf count) 0.5 a)))
	                    (alexandria::nconcf rules (list new-rule)))
		          (when max-rep
                            (let ((new-rule (list (make-list max-rep :initial-element (- count 1)) count 1.0)))
	                      (alexandria::nconcf rules (list new-rule)))))
                        (let ((new-rule (list (list count) (incf count) 1.0 a)))
	                  (alexandria::nconcf rules (list new-rule))))
                    (let ((new-rule (list (list count) (incf count) 1.0 a)))
	              (alexandria::nconcf rules (list new-rule)))))))
    (if (typep (car (last real-events)) 'number)
	(setf rules (nconc rules (list (list (list count) 1 1.0 (car (last real-events))))))
	(setf rules (nconc rules (list (list (list count) 1 1.0)))))
    (list event-mapping rules)))

(defun cyc (name cyc-def &key (rep 0) (max-rep 2) (dur *global-default-duration*))
  (let ((gen-ev (parse-cycle cyc-def :rep rep :max-rep max-rep :dur dur)))
    (infer-from-rules :type 'naive :name name :mapping (car gen-ev) :rules (cadr gen-ev) :default-dur dur)))

(defun cyc2 (name cyc-def &key (rep 0) (max-rep 2) (dur *global-default-duration*))
  (let ((gen-ev (parse-cycle cyc-def :rep rep :max-rep max-rep :dur dur)))
    (infer-from-rules :type 'pfa :name name :mapping (car gen-ev) :rules (cadr gen-ev) :default-dur dur)))

(defun nuc (name event &key (dur *global-default-duration*))  
  (infer-from-rules :type 'naive
                    :name name
                    :mapping (alexandria::plist-hash-table (list 1 (list event)))
	            :rules (list (list '(1) 1 100 dur))
	            :default-dur dur))

(defun nuc2 (name event &key (dur *global-default-duration*))  
  (infer-from-rules :type 'pfa
                    :name name
                    :mapping (alexandria::plist-hash-table (list 1 (list event)))
	            :rules (list (list '(1) 1 1.0 dur))
	            :default-dur dur))




