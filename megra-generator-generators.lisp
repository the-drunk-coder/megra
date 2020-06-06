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
		          (setf stack (nconc stack (list (cond ((equal (char token 0) #\')
                                                                (list (read-from-string (format nil "~D" (subseq token 1)))))
                                                               (t (let ((f-par (cl-ppcre:split ":" token)))  
							            (eval (read-from-string (format nil "(~{~a~^ ~})" f-par)))))))))
		          (setf cycle (nconc cycle (list (cond ((equal (char token 0) #\')
                                                                (list (read-from-string (format nil "~D" (subseq token 1)))))
                                                               (t (let ((f-par (cl-ppcre:split ":" token)))  
							            (eval (read-from-string (format nil "(~{~a~^ ~})" f-par)))))))))))))
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

(defun cyc (name cyc-def &rest rest)
  (let* ((rep (find-keyword-val :rep rest :default 0))
         (max-rep (find-keyword-val :max-rep rest :default 2))
         (rnd (find-keyword-val :rnd rest :default 0))         
         (dur (find-keyword-val :dur rest :default *global-default-duration*))
         (reset (find-keyword-val :reset rest :default t))
         (map (find-keyword-val :map rest :default nil))
         (filters (find-keyword-list :for rest)) 
         (successor (if (typep (alexandria::lastcar rest) 'function)
                        (alexandria::lastcar rest)))
         (gen-ev (parse-cycle cyc-def :rep rep :max-rep max-rep :dur dur)))
    (if map
        (mapev map (infer-from-rules-fun :type 'naive :name name :mapping (car gen-ev) :rnd rnd :rules (cadr gen-ev) :default-dur dur
                                         :reset reset :successor successor :combine-filter (if filters (multi-filter filters) 'all-p)))

        (infer-from-rules-fun :type 'naive :name name :mapping (car gen-ev) :vis-hint 'circle :rnd rnd :rules (cadr gen-ev) :default-dur dur 
                              :reset reset :successor successor :combine-filter (if filters (multi-filter filters) 'all-p)))))

(defun cyc2 (name cyc-def &rest rest)
  (let* ((rep (find-keyword-val :rep rest :default 0))
         (max-rep (find-keyword-val :max-rep rest :default 2))
         (rnd (find-keyword-val :rnd rest :default 0))
         (dur (find-keyword-val :dur rest :default *global-default-duration*))
         (reset (find-keyword-val :reset rest :default t))
         (map (find-keyword-val :map rest :default nil))
         (filters (find-keyword-list :for rest))         
         (successor (if (typep (alexandria::lastcar rest) 'function)
                        (alexandria::lastcar rest)))
         (gen-ev (parse-cycle cyc-def :rep rep :max-rep max-rep :dur dur)))
    (if map
        (mapev map (infer-from-rules-fun :type 'pfa :name name :mapping (car gen-ev) :vis-hint 'circle :rules (cadr gen-ev) :rnd rnd :default-dur dur
                                         :reset reset :successor successor :combine-filter (if filters (multi-filter filters) 'all-p)))
        (infer-from-rules-fun :type 'pfa :name name :mapping (car gen-ev) :rules (cadr gen-ev) :rnd rnd :default-dur dur
                              :reset reset :successor successor :combine-filter (if filters (multi-filter filters) 'all-p)))))

(defun nuc (name event &rest rest)
  (let ((dur (find-keyword-val :dur rest :default *global-default-duration*))
        (reset (find-keyword-val :reset rest :default t))
        (filters (find-keyword-list :for rest))         
        (successor (if (typep (alexandria::lastcar rest) 'function)
                       (alexandria::lastcar rest))))
    (infer-from-rules-fun :type 'naive
                          :name name
                          :vis-hint 'cose
                          :mapping (alexandria::plist-hash-table (list 1 (alexandria::flatten (list event))))
	                  :rules (list (list '(1) 1 100 dur))
	                  :default-dur dur
                          :reset reset
                          :successor successor
                          :combine-filter (if filters (multi-filter filters) 'all-p))))

(defun nuc2 (name event &rest rest)
  (let ((dur (find-keyword-val :dur rest :default *global-default-duration*))
        (reset (find-keyword-val :reset rest :default t))
        (filters (find-keyword-list :for rest))         
        (successor (if (typep (alexandria::lastcar rest) 'function)
                       (alexandria::lastcar rest))))
    (infer-from-rules-fun :type 'pfa
                          :name name
                          :vis-hint 'cose
                          :mapping (alexandria::plist-hash-table (list 1 (alexandria::flatten (list event))))
	                  :rules (list (list '(1) 1 100 dur))
	                  :default-dur dur
                          :reset reset
                          :successor successor
                          :combine-filter (if filters (multi-filter filters) 'all-p))))

(defun infer (name &rest params)
  "infer a generator from rules"
  (let ((events (find-keyword-list :events params))
        (rules (find-keyword-list :rules params))
        (dur (find-keyword-val :dur params :default *global-default-duration*))
        (type (find-keyword-val :type params :default 'pfa))
        (reset (find-keyword-val :reset params :default t))
        (filters (find-keyword-list :for params))
        (successor (if (typep (alexandria::lastcar params) 'function)
                       (alexandria::lastcar params))))
    (infer-from-rules-fun :type type 
                          :name name
                          :mapping (p-events-list events)
	                  :rules rules
                          :vis-hint 'cose
	                  :default-dur dur
                          :reset reset
                          :successor successor
                          :combine-filter (if filters (multi-filter filters) 'all-p))))

(defun sstring (string-as-sym)
  "convenience method to enter sample strings without spaces"
  (let ((sname (if (typep string-as-sym 'string)
		   string-as-sym
		   (symbol-name string-as-sym))))
    (loop for c in (coerce sname 'list)
          collecting (intern (string-upcase (string c))))))

(defun learn (name &rest params)
  "lear a generator from a sample"
  (let* ((reset (find-keyword-val :reset params :default t))
         (bound (find-keyword-val :bound params :default 3))
         (size (find-keyword-val :size params :default 40))
         (filters (find-keyword-list :for params))
         (epsilon (find-keyword-val :epsilon params :default 0.01))
         (dur (find-keyword-val :dur params :default *global-default-duration*))         
         (successor (if (typep (alexandria::lastcar params) 'function)
                        (alexandria::lastcar params)))
         (sample (if successor
                     (alexandria::lastcar (butlast params))
                     (alexandria::lastcar params)))
         (events (delete successor (delete sample (find-keyword-list :events params) :test 'equal) :test 'equal)))
    (learn-generator-fun :name name
                         :sample (if (listp sample) sample (sstring sample))
                         :size size
                         :epsilon epsilon
                         :bound bound
                         :reset reset                     
                         :mapping (p-events-list events)
                         :default-dur dur
                         :successor successor
                         :combine-filter (if filters (multi-filter filters) 'all-p))))

(defun fully-connected (events)
  (let ((count 1)
        (rep-prob (ceiling (/ 100 (length events))))
        (gen-prob (floor (/ 100 (length events))))
        (rules (list))
        (event-mapping (make-hash-table :test #'equal)))
    (loop for ev in events
          do (setf (gethash count event-mapping) (nconc (gethash count event-mapping) (list ev)))
          do (loop for i from 1 to (length events)
                   when (not (eql i count))
                   do (push (list (list count) i gen-prob) rules))
          do (push (list (list count) count rep-prob) rules)
          do (incf count))
    (list event-mapping rules)))

(defun fully (name &rest rest)
  (let* ((filters (find-keyword-list :for rest))
         (reset (find-keyword-val :reset rest :default nil))
         (dur (find-keyword-val :dur rest :default *global-default-duration*))         
         (successor (if (typep (alexandria::lastcar rest) 'function)
                        (alexandria::lastcar rest)))
         (events (delete nil (mapcar #'(lambda (e) (if (typep e 'event) e)) rest)))
         (gen-ev (fully-connected events)))    
    (infer-from-rules-fun :type 'naive :name name :vis-hint 'circle :mapping (car gen-ev) :rules (cadr gen-ev) :default-dur dur
                          :reset reset :successor successor :combine-filter (if filters (multi-filter filters) 'all-p))))

(defun fully2 (name &rest rest)
  (let* ((filters (find-keyword-list :for rest))
         (reset (find-keyword-val :reset rest :default nil))
         (dur (find-keyword-val :dur rest :default *global-default-duration*))
         (successor (if (typep (alexandria::lastcar rest) 'function)
                        (alexandria::lastcar rest)))
         (events (delete nil (mapcar #'(lambda (e) (if (typep e 'event) e)) rest)))
         (gen-ev (fully-connected events))) 
    (infer-from-rules-fun :type 'pfa :name name :vis-hint 'circle :mapping (car gen-ev) :rules (cadr gen-ev) :default-dur dur
                          :reset reset :successor successor :combine-filter (if filters (multi-filter filters) 'all-p))))

(defun friendship-connected (events)
  (let* ((count 1)
         (center (car events))
         (outer (cdr events))
         (outer-pad (if (oddp (length outer)) (nconc outer (list (deepcopy (alexandria::lastcar outer)))) outer))
         (center-exit-prob (round (/ 100 (/ (length outer-pad) 2))))
         (rules (list))
         (event-mapping (make-hash-table :test #'equal)))
    (setf (gethash count event-mapping) (list center))
    (loop for (a b) on outer-pad by #'cddr
          do (setf (gethash (+ count 1) event-mapping) (list a))
          do (setf (gethash (+ count 2) event-mapping) (list b))
          do (push (list (list 1) (+ count 1) center-exit-prob) rules)
          do (push (list (list (+ count 1)) (+ count 2) 100) rules)
          do (push (list (list (+ count 2)) 1 100) rules)
          do (setf count (+ count 2)))    
    (list event-mapping rules)))

(defun friendship (name &rest rest)
  (let* ((filters (find-keyword-list :for rest))
         (reset (find-keyword-val :reset rest :default t))
         (dur (find-keyword-val :dur rest :default *global-default-duration*))         
         (successor (if (typep (alexandria::lastcar rest) 'function)
                        (alexandria::lastcar rest)))
         (events (delete nil (mapcar #'(lambda (e) (if (typep e 'event) e)) rest)))
         (gen-ev (friendship-connected events)))    
    (infer-from-rules-fun :type 'naive :name name :vis-hint 'cose :mapping (car gen-ev) :rules (cadr gen-ev) :default-dur dur
                          :reset reset :successor successor :combine-filter (if filters (multi-filter filters) 'all-p))))

(defun friendship2 (name &rest rest)
  (let* ((filters (find-keyword-list :for rest))
         (reset (find-keyword-val :reset rest :default t))
         (dur (find-keyword-val :dur rest :default *global-default-duration*))
         (successor (if (typep (alexandria::lastcar rest) 'function)
                        (alexandria::lastcar rest)))
         (events (delete nil (mapcar #'(lambda (e) (if (typep e 'event) e)) rest)))
         (gen-ev (friendship-connected events))) 
    (infer-from-rules-fun :type 'pfa :name name :vis-hint 'cose :mapping (car gen-ev) :rules (cadr gen-ev) :default-dur dur
                          :reset reset :successor successor :combine-filter (if filters (multi-filter filters) 'all-p))))

(defun flower-connected (events layers)
  (let* ((center (car events))
         (outer (cdr events))
         (outer-pad (if (not (eq (rem (length outer) layers) 0))
                        (nconc outer (make-list (rem (length outer) layers) :initial-element (deepcopy (alexandria::lastcar outer))))
                        outer))
         (center-exit-prob (round (/ 100 (/ (length outer-pad) layers))))
         (rules (list))
         (event-mapping (make-hash-table :test #'equal)))
    (setf (gethash 0 event-mapping) (list center))
    (loop for i from 1 to (- (+ 1 (length outer-pad)) layers) by layers
          do (push (list (list 0) i center-exit-prob) rules)
          do (loop for l from 0 to (- layers 1)
                   do (setf (gethash (+ i l) event-mapping) (list (nth (- (+ i l) 1) outer-pad)))
                   do (let ((next (if (< l (- layers 1)) (+ i l 1) nil))
                            (prev (if (eq l 0) 0 (- (+ i l) 1))))
                        (push (list (list (+ i l)) prev (if next 50 100)) rules)
                        (if next (push (list (list (+ i l)) next 50) rules)))))    
    (list event-mapping rules)))

(defun flower (name &rest rest)
  (let* ((filters (find-keyword-list :for rest))
         (reset (find-keyword-val :reset rest :default t))
         (layers (find-keyword-val :layers rest :default 1))
         (dur (find-keyword-val :dur rest :default *global-default-duration*))         
         (successor (if (typep (alexandria::lastcar rest) 'function)
                        (alexandria::lastcar rest)))
         (events (delete nil (mapcar #'(lambda (e) (if (typep e 'event) e)) rest)))
         (gen-ev (flower-connected events layers)))    
    (infer-from-rules-fun :type 'naive :name name :vis-hint 'cose :mapping (car gen-ev) :rules (cadr gen-ev) :default-dur dur
                          :reset reset :successor successor :combine-filter (if filters (multi-filter filters) 'all-p))))

(defun flower2 (name &rest rest)
  (let* ((filters (find-keyword-list :for rest))
         (reset (find-keyword-val :reset rest :default t))
         (layers (find-keyword-val :layers rest :default 1))
         (dur (find-keyword-val :dur rest :default *global-default-duration*))
         (successor (if (typep (alexandria::lastcar rest) 'function)
                        (alexandria::lastcar rest)))
         (events (delete nil (mapcar #'(lambda (e) (if (typep e 'event) e)) rest)))
         (gen-ev (flower-connected events layers))) 
    (infer-from-rules-fun :type 'pfa :name name :vis-hint 'cose :mapping (car gen-ev) :rules (cadr gen-ev) :default-dur dur
                          :reset reset :successor successor :combine-filter (if filters (multi-filter filters) 'all-p))))


;;;;;;;;;;;;; SOME SHORTHANDS ;;;;;;;;;;;;;;;;;;;

;; parameter sequence
(defmacro pseq (name param &rest rest)
  (let ((p-events (loop for val in rest collect `(,param ,val)))
        (filters (find-keyword-list :for rest)))
    `(funcall (lambda ()
                (let* ((raw-succ ,(alexandria::lastcar rest))
                       (successor (if (or (typep raw-succ 'event-processor) (typep raw-succ 'function)) successor)))
                  (cyc ,name (list ,@p-events) successor :for ,@filters))))))

;; chop a sample
(defmacro chop (name template num &rest rest)
  (let* ((start (find-keyword-val :start rest :default 0.0))
         (p-events (loop for val from 0 to num
		         collect `(let ((cur-ev ,template))                                   
                                    (setf (event-start cur-ev) (+ ,start (* ,val (coerce (/  (- 1.0 ,start) ,num) 'float))))
                                    cur-ev))))
    `(funcall (lambda ()
                (let* ((raw-succ ,(alexandria::lastcar rest))
                       (successor (if (or (typep raw-succ 'event-processor) (typep raw-succ 'function)) successor)))
                  (cyc ,name (list ,@p-events) successor))))))

(defmacro chop2 (name template num &rest rest)
  (let* ((start (find-keyword-val :start rest :default 0.0))
         (p-events (loop for val from 0 to num
		         collect `(let ((cur-ev ,template))                                   
                                    (setf (event-start cur-ev) (+ ,start (* ,val (coerce (/  (- 1.0 ,start) ,num) 'float))))
                                    cur-ev))))
    `(funcall (lambda ()
                (let* ((raw-succ ,(alexandria::lastcar rest))
                       (successor (if (or (typep raw-succ 'event-processor) (typep raw-succ 'function)) successor)))
                  (cyc2 ,name (list ,@p-events) successor))))))


