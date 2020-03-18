(in-package :megra)

(defclass generator (event-processor)
  ((name :accessor name :initarg :name)
   (inner-generator :accessor inner-generator :initarg :generator)
   (combine-filter :accessor combine-filter :initarg :combine-filter :initform #'all-p)
   (symbol-ages :accessor ages :initarg :ages :initform (make-hash-table :test 'equal)) ;; move this to core model, so that all operations can be defined there !
   (transition-durations :accessor transition-durations :initform (make-hash-table :test #'equal))
   (event-dictionary :accessor event-dictionary :initarg :events :initform (make-hash-table :test #'equal))   
   (default-duration :accessor default-duration :initarg :default-duration :initform 0)
   (last-transition :accessor last-transition :initarg :last-transition :initform (vom::make-query-result))))

;; tagging - this would need a name for the generator ? - also, the symbol could be added to the tags

(defmethod current-events ((g generator) &key)
  ;; increment symbol age ...
  (incf (gethash (vom::query-result-symbol (last-transition g)) (ages g)))
  (let ((cev (deepcopy (gethash (vom::query-result-symbol (last-transition g)) (event-dictionary g)))))
    (loop for ev in cev do (push (name g) (event-tags ev)))
    ;; unique source id, as in old graphs ??
    cev))

(defmethod current-transition ((g generator) &key)
  (setf (last-transition g) (vom::next-transition (inner-generator g)))
  (let* ((key1 (cons (vom::query-result-last-state (last-transition g))
                     (vom::query-result-symbol (last-transition g))))
         (key2 (cons (last (vom::query-result-last-state (last-transition g)))
                     (vom::query-result-symbol (last-transition g))))
         (dur (if (gethash key1 (transition-durations g))
                  (gethash key1 (transition-durations g)) ;; try direct
                  (gethash key2 (transition-durations g))))
         (tr (make-instance 'transition-event :dur (if dur dur (default-duration g)) :tags '(transition))))
    ;; add tag
    (push (name g) (event-tags tr))
    (list tr)))

(defun infer-naive (name mapping default-dur rules)
  (let* ((normalized-rules (mapc #'(lambda (r) (if (floatp (nth 2 r)) (setf (nth 2 r) (floor (* (nth 2 r) 100))))) rules))
         (g (make-instance 'generator :name name :generator (vom::infer-naive-pfa-list normalized-rules) :events mapping :default-duration default-dur)))
    ;; keep track of symbol ages ...
    (setf (last-transition g) (vom::make-query-result :symbol (alexandria::lastcar (vom::history (inner-generator g)))))
    (mapc #'(lambda (s) (setf (gethash s (ages g)) 0)) (vom::alphabet (inner-generator g)))
    (loop for rule in rules 
          when (nth 3 rule)
          do (setf
              (gethash (cons (car rule) (if (listp (nth 1 rule)) (car (nth 1 rule)) (nth 1 rule)))   
                       (transition-durations g))
              (nth 3 rule)))
    g))

(defun infer-adj-pfa (name mapping default-dur rules)
  (let* ((normalized-rules (mapc #'(lambda (r) (if (integerp (nth 2 r)) (setf (nth 2 r) (coerce (/ (nth 2 r) 100) 'float)))) rules))
         (g (make-instance 'generator :name name :generator (vom::infer-adj-list-pfa-list normalized-rules) :events mapping :default-duration default-dur)))    
    (setf (vom::current-state (inner-generator g)) (caar rules))
    (setf (last-transition g) (vom::make-query-result :symbol (caaar rules)))
    ;; keep track of symbol ages ...
    (mapc #'(lambda (s) (setf (gethash s (ages g)) 0)) (vom::alphabet (inner-generator g)))
    ;; generate durations ...
    (loop for rule in rules 
          when (nth 3 rule)
          do (setf
              (gethash (cons (car rule) (if (listp (nth 1 rule)) (car (nth 1 rule)) (nth 1 rule)))  
                       (transition-durations g))
              (nth 3 rule)))
    g))

(defun infer-generator (name type mapping default-dur rules)
  (cond ((equal type 'naive) (infer-naive name mapping default-dur rules))
        ((equal type 'pfa) (infer-adj-pfa name mapping default-dur rules))
        (t (infer-naive name mapping default-dur rules))))

(defun infer-from-rules (&key type name events rules mapping (default-dur *global-default-duration*) reset)  
  "infer a generator from rules"
  (define-filter name)
  (let* ((event-mapping (if mapping mapping (alexandria::plist-hash-table events))) ;; mapping has precedence         
         (g-old (gethash name *processor-directory*))
         (g (if (or (not g-old) (and g-old reset))
                (infer-generator name type event-mapping default-dur rules))))
    (setf (gethash *global-silence-symbol* mapping) (list (silence)))
    ;; state preservation, if possible
    (when (and g g-old)
      (setf (last-transition g) (last-transition g-old))
      (vom::transfer-state (inner-generator g-old) (inner-generator g)))    
    (if g
        (progn (setf (gethash name *processor-directory*) g) g)
        g-old)))


(defun learn-generator (&key name events sample mapping (size 40) (bound 3) (epsilon 0.01) (default-dur *global-default-duration*) (reset t))  
  "infer a generator from rules"
  (define-filter name)
  (let* ((event-mapping (if mapping mapping (alexandria::plist-hash-table events))) ;; mapping has precedence         
         (g-old (gethash name *processor-directory*))
         (g (if (or (not g-old) (and g-old reset))
                (make-instance 'generator :name name
                                          :generator (vom::learn-adj-list-pfa
                                                      (delete-duplicates sample)
                                                      bound
                                                      epsilon
                                                      size
                                                      sample)
                                          :events mapping
                                          :default-duration default-dur))))
    (setf (gethash *global-silence-symbol* mapping) (list (silence)))
    ;; state preservation, if possible
    (when (and g g-old)
      (setf (last-transition g) (last-transition g-old))
      (vom::transfer-state (inner-generator g-old) (inner-generator g)))    
    (if g
        (progn
          (setf (last-transition g) (vom::make-query-result :symbol (car (vom::alphabet (inner-generator g)))))
          (mapc #'(lambda (s) (setf (gethash s (ages g)) 0)) (vom::alphabet (inner-generator g)))
          (setf (gethash name *processor-directory*) g)
          g)
        g-old)))

(defun to-svg (graph-or-id)
  (let* ((g (if (typep graph-or-id 'symbol)
	       (gethash graph-or-id *processor-directory*)
	       graph-or-id))
         (ig (inner-generator g)))
    (if (typep ig 'vom::adj-list-pfa)
        (vom::adj-list-pfa->svg ig (symbol-name (name g)))
        (vom::graph->svg ig (symbol-name (name g))))))
