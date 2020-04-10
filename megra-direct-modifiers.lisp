(in-package :megra)

;; FUNCTIONS that modify the PFAs or their evaluations order ...

;; SKIP
(defun skip (num &optional proc)    
  (if proc
      (if (typep proc 'function)
          (lambda (&optional nproc) (skip num (funcall proc nproc)))
          (progn
            (loop for a from 0 to (- num 1)
                  do (progn                              
                       (pull-events proc :skip-successor t)
                       (pull-transition proc :skip-successor t)))
            proc))
      (lambda (nproc) (skip num nproc))))

(defun inner-grown (n var rest proc)
  (if (typep proc 'function)
      (lambda (&optional nproc) (inner-grown n var rest (funcall proc nproc)))
      (let ((method (find-keyword-val :method rest :default 'triloop))
	    (variance (find-keyword-val :var rest :default 0.2))	    
	    (durs (find-keyword-val :durs rest :default nil))
	    (hoe-max (find-keyword-val :hoe-max rest :default 0))
	    ;;(hoe (find-keyword-val :hoe rest :default 4))
            (rnd (find-keyword-val :rnd rest :default 0)))
        (progn (loop for a from 0 to n
                     do (grow proc :higher-order hoe-max :rnd rnd :var variance :method method :durs durs))
               proc))))

;; GROWN
(defun grown (n var &rest opt-params)
  (let* ((proc (if (or (typep (alexandria::lastcar opt-params) 'event-processor)
                       (typep (alexandria::lastcar opt-params) 'function))
                   (alexandria::lastcar opt-params)
                   nil))
         (params (if proc (butlast opt-params) opt-params)))
    (if proc
        (inner-grown n var params proc)
        (lambda (pproc) (inner-grown n var params pproc)))))

;; haste 4 0.5 - apply tempo mod for the next n times (only on base proc)
(defun haste (num mod &optional proc)  
  (if proc
      (if (typep proc 'function)
          (lambda (&optional nproc) (haste num mod (funcall proc nproc)))
          (progn (loop for a from 0 to (- num 1)
                       do (push-tmod proc mod))
                 proc))
      (lambda (nproc) (haste num mod nproc))))

;; relax 4 0.5 - apply tempo mod for the next n times (only on base proc)
(defun relax (num mod &optional proc)  
  (if proc
      (if (typep proc 'function)
          (lambda (&optional nproc) (relax num mod (funcall proc nproc)))
          (progn (loop for a from 0 to (- num 1)
                       do (push-tmod proc (coerce (/ 1.0 mod) 'float)))
                 proc))
      (lambda (nproc) (relax num mod nproc))))

;; rew 3 - rewind (set to state n back in traced path)
(defun rew (num &optional proc)  
  (if proc
      (if (typep proc 'function)
          (lambda (&optional nproc) (rew num (funcall proc nproc)))
          (progn            
            (if (typep (inner-generator proc) 'vom::adj-list-pfa) 
                (progn
                  (setf (vom::current-state (inner-generator proc))
                        (list (nth (- (vom::history-length (inner-generator proc)) (+ num 1)) (vom::history (inner-generator proc)))))
                  (setf (vom::history (inner-generator proc))
                        (append (vom::history (inner-generator proc)) (vom::current-state (inner-generator proc)))))
                (progn
                  (setf (vom::current-node (inner-generator proc))
                        (nth (- (vom::history-length (inner-generator proc)) (+ num 1)) (vom::history (inner-generator proc))))
                  (setf (vom::history (inner-generator proc))
                        (append (vom::history (inner-generator proc)) (list (vom::current-node (inner-generator proc)))))))                        
            proc))
      (lambda (nproc) (rew num nproc))))

(defun rep (prob max &optional proc)  
  (if proc
      (if (typep proc 'function)
          (lambda (&optional nproc) (rep prob max (funcall proc nproc)))         
          (progn
            (loop for sym in (vom::alphabet (inner-generator proc))
                  do (let ((next (if (cadr (member sym (vom::alphabet (inner-generator proc))))
                                     (cadr (member sym (vom::alphabet (inner-generator proc))))
                                     (car (vom::alphabet (inner-generator proc))))))
                       (when (and (not (vom::has-transition (inner-generator proc) (list sym) sym))
                                  (< (random 100) prob))
                         (vom::insert-rule (inner-generator proc) (list (list sym) sym (* prob 0.01)))
                         ;;(format t "RULE ~D~% " (list (list sym) sym (* prob 0.01)))
                         (vom::insert-rule (inner-generator proc) (list (make-list max :initial-element sym)
                                                                        next 1.0))
                         ;;(format t "MAX RULE ~D~% " (list (make-list max :initial-element sym) next 1.0))
                         (vom::rebalance-state (inner-generator proc) (list sym)))))
            proc))
      (lambda (nproc) (rep prob max nproc))))

(defun sharpen (factor &optional proc)  
  (if proc
      (if (typep proc 'function)
          (lambda (&optional nproc) (sharpen factor (funcall proc nproc)))         
          (vom::sharpen-pfa (inner-generator proc) factor))
      (lambda (nproc) (sharpen factor nproc))))

(defun blur (factor &optional proc)  
  (if proc
      (if (typep proc 'function)
          (lambda (&optional nproc) (blur factor (funcall proc nproc)))         
          (vom::blur-pfa (inner-generator proc) factor))
      (lambda (nproc) (blur factor nproc))))

(defun discourage (factor &optional proc)  
  (if proc
      (if (typep proc 'function)
          (lambda (&optional nproc) (discourage factor (funcall proc nproc)))
          (vom::discourage-pfa (inner-generator (if (symbolp proc) (gethash proc *processor-directory*) proc)) factor))
      (lambda (nproc) (discourage factor nproc))))

(defun encourage (factor &optional proc)  
  (if proc
      (if (typep proc 'function)
          (lambda (&optional nproc) (encourage factor (funcall proc nproc)))         
          (vom::encourage-pfa (inner-generator (if (symbolp proc) (gethash proc *processor-directory*) proc)) factor))
      (lambda (nproc) (encourage factor nproc))))

(defun rnd (chance &optional proc)
  (if proc
      (if (typep proc 'function)
          (lambda (&optional nproc) (rnd chance (funcall proc nproc)))         
          (vom::randomize-edges (inner-generator (if (symbolp proc) (gethash proc *processor-directory*) proc)) chance))
      (lambda (nproc) (rnd chance nproc))))


