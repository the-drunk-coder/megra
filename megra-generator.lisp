(in-package :megra)

(defclass generator (event-processor)
  ((name :accessor name :initarg :name)
   (inner-generator :accessor inner-generator :initarg :generator)
   (combine-filter :accessor combine-filter :initarg :combine-filter :initform #'all-p)
   (symbol-ages :accessor ages :initarg :ages :initform (make-hash-table :test 'equal)) ;; move this to core model, so that all operations can be defined there !
   (transition-durations :accessor transition-durations :initform (make-hash-table :test #'equal))
   (event-dictionary :accessor event-dictionary :initarg :events :initform (make-hash-table :test #'equal))   
   (default-duration :accessor default-duration :initarg :default-duration :initform 0)
   (last-transition :accessor last-transition)))

;; tagging - this would need a name for the generator ? - also, the symbol could be added to the tags

(defmethod current-events ((g generator) &key)
  ;; add name, type etc as tags ??
  (deepcopy (gethash (vom::query-result-symbol (last-transition g)) (event-dictionary g))))

(defmethod current-transition ((g generator) &key)
  (setf (last-transition g) (vom::next-transition (inner-generator g)))
  (let ((dur (gethash (cons (vom::query-result-last-state (last-transition g))
                            (vom::query-result-current-state (last-transition g)))
                      (transition-durations g))))
    (list (make-instance 'transition-event :dur (if dur dur (default-duration g)) :tags '(transition)))))

(defun infer-naive (name mapping default-dur rules)
  (let* ((normalized-rules (mapc #'(lambda (r) (if (floatp (nth 2 r)) (setf (nth 2 r) (floor (* (nth 2 r) 100))))) rules))
         (g (make-instance 'generator :name name :generator (vom::infer-naive-pfa-list normalized-rules) :events mapping :default-duration default-dur)))
    ;; keep track of symbol ages ...
    (setf (last-transition g) (vom::make-query-result :symbol (alexandria::lastcar (vom::history (inner-generator g)))))
    (mapc #'(lambda (s) (setf (gethash s (ages g)) 0)) (vom::alphabet (inner-generator g)))
    (loop for rule in rules 
          when (nth 3 rule)
          do (setf
              (gethash (cons (car rule) (if (listp (nth 1 rule)) (nth 1 rule) (list (nth 1 rule))))  
                       (transition-durations g))
              (nth 3 rule)))
    g))

(defun infer-st-pfa (name mapping default-dur rules)
  (let* ((normalized-rules (mapc #'(lambda (r) (if (integerp (nth 2 r)) (setf (nth 2 r) (coerce (/ (nth 2 r) 100) 'float)))) rules))
         (g (make-instance 'generator :name name :generator (vom::infer-st-pfa-list normalized-rules) :events mapping :default-duration default-dur)))
    (vom::pfa-set-current-state (inner-generator g) (caar rules))
    (setf (last-transition g) (vom::make-query-result :symbol (caar rules)))
    ;; keep track of symbol ages ...
    (mapc #'(lambda (s) (setf (gethash s (ages g)) 0)) (vom::alphabet (inner-generator g)))
    (loop for rule in rules 
          when (nth 3 rule)
          do (setf
              (gethash (cons (car rule) (if (listp (nth 1 rule)) (nth 1 rule) (list (nth 1 rule))))  
                       (transition-durations g))
              (nth 3 rule)))
    g))

(defun infer-generator (name type mapping default-dur rules)
  (cond ((equal type 'naive) (infer-naive name mapping default-dur rules))
        ((equal type 'pfa) (infer-st-pfa name mapping default-dur rules))
        (t (infer-naive name mapping default-dur rules))))

(defun infer-from-rules (&key type name events rules mapping (default-dur *global-default-duration*))  
  "infer a generator from rules"
  (define-filter name)
  (let* ((event-mapping (alexandria::plist-hash-table events))
         (g (infer-generator name type event-mapping default-dur rules)))
    (setf (gethash name *processor-directory*) g)
    g))

;; found format: key: (s . d) value dur
(defun create-duration-mapping (transitions found-durations)  
  (let ((duration-map (make-hash-table :test 'equal)))
    (mapc #'(lambda (tr)
              (let ((key (cons (car (last (car tr))) (caadr tr))))
                
                (when (gethash key found-durations)
                  (setf (gethash tr duration-map) (gethash key found-durations)))))
          transitions)    
    duration-map))
