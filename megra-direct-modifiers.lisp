(in-package :megra)

;; FUNCTIONS that modify the PFAs or their evaluations order ...

;; SKIP
(defun skip (num &optional proc)    
  (if proc
      (if (typep proc 'function)
          (lambda (&optional nproc) (skip num (funcall proc nproc)))
          (let ((iproc (if (symbolp proc) (gethash proc *processor-directory*) proc)))
            (loop for a from 0 to (- num 1)
                  do (progn                              
                       (pull-events iproc :skip-successor t)
                       (pull-transition iproc :skip-successor t)))
            iproc))
      (lambda (nproc) (skip num nproc))))

(defun inner-grown (n rest proc)
  (if (typep proc 'function)
      (lambda (&optional nproc) (inner-grown n var rest (funcall proc nproc)))
      (let ((method (find-keyword-val :method rest :default 'triloop))
	    (variance (find-keyword-val :var rest :default 0.2))	    
	    (durs (find-keyword-val :durs rest :default nil))
	    (hoe-max (find-keyword-val :hoe-max rest :default 0))
	    ;;(hoe (find-keyword-val :hoe rest :default 4))
            (rnd (find-keyword-val :rnd rest :default 0)))
        (progn (loop for a from 0 to n
                     do (grow-generator proc :higher-order hoe-max :rnd rnd :var variance :method method :durs durs))
               proc))))

;; GROWN
(defun grown (n &rest opt-params)
  (let* ((last (alexandria::lastcar opt-params))
         (proc (if (or (typep last 'event-processor)
                       (typep last 'function)
                       (typep last 'symbol))
                   (if (typep last 'symbol)
                       (gethash last *processor-directory*)
                       last)
                   nil))
         (params (if proc (butlast opt-params) opt-params)))
    (if proc
        (inner-grown n params proc)
        (lambda (pproc) (inner-grown n params pproc)))))

;; GROWN
(defun grow (&rest opt-params)
  (let* ((last (alexandria::lastcar opt-params))
         (proc (if (or (typep last 'event-processor)
                       (typep last 'function)
                       (typep last 'symbol))
                   (if (typep last 'symbol)
                       (gethash last *processor-directory*)
                       last)
                   nil))
         (params (if proc (butlast opt-params) opt-params)))
    (if proc
        (inner-grown 1 params proc)
        (lambda (pproc) (inner-grown 1 params pproc)))))

(defun shrink (&rest params)  
  (let* ((last (alexandria::lastcar params))
         (proc (if (or (typep last 'event-processor)
                       (typep last 'function)
                       (typep last 'symbol))
                   (if (typep last 'symbol)
                       (gethash last *processor-directory*)
                       last)
                  nil)))
    (if proc
        (if (typep proc 'function)
            (lambda (&optional nproc) (apply 'shrink (nconc params (list (funcall proc nproc)))))
            (let ((node-id (find-keyword-val :node-id params :default nil))
                  (exclude (find-keyword-val :exclude params :default nil))
                  (iproc (if (symbolp proc) (gethash proc *processor-directory*) proc)))
              (when (> (length (vom::alphabet (inner-generator iproc))) 1) ;; don't let it die
	        (let ((rnd-symbol (alexandria::random-elt (vom::alphabet (inner-generator iproc)))))                  
                  (prune-generator iproc :node-id (if node-id node-id rnd-symbol))))
              iproc))
        (lambda (nproc) (apply 'shrink (nconc params (list nproc)))))))


;; haste 4 0.5 - apply tempo mod for the next n times (only on base proc)
(defun haste (num mod &optional proc)  
  (if proc
      (if (typep proc 'function)
          (lambda (&optional nproc) (haste num mod (funcall proc nproc)))
          (let ((iproc (if (symbolp proc) (gethash proc *processor-directory*) proc))) 
            (loop for a from 0 to (- num 1) do (push-tmod iproc mod)) iproc))
      (lambda (nproc) (haste num mod nproc))))

;; relax 4 0.5 - apply tempo mod for the next n times (only on base proc)
(defun relax (num mod &optional proc)  
  (if proc
      (if (typep proc 'function)
          (lambda (&optional nproc) (relax num mod (funcall proc nproc)))
          (let ((iproc (if (symbolp proc) (gethash proc *processor-directory*) proc)))            
            (loop for a from 0 to (- num 1) do (push-tmod iproc (coerce (/ 1.0 mod) 'float))) iproc))
      (lambda (nproc) (relax num mod nproc))))

;; rew 3 - rewind (set to state n back in traced path)
(defun rew (num &optional proc)  
  (if proc
      (if (typep proc 'function)
          (lambda (&optional nproc) (rew num (funcall proc nproc)))
          (let ((iproc (if (symbolp proc) (gethash proc *processor-directory*) proc)))            
            (if (typep (inner-generator iproc) 'vom::adj-list-pfa) 
                (progn
                  (setf (vom::current-state (inner-generator iproc))
                        (list (nth (- (vom::history-length (inner-generator iproc)) (+ num 1)) (vom::history (inner-generator iproc)))))
                  (setf (vom::history (inner-generator iproc))
                        (append (vom::history (inner-generator iproc)) (vom::current-state (inner-generator iproc)))))
                (progn
                  (setf (vom::current-node (inner-generator iproc))
                        (nth (- (vom::history-length (inner-generator iproc)) (+ num 1)) (vom::history (inner-generator iproc))))
                  (setf (vom::history (inner-generator iproc))
                        (append (vom::history (inner-generator iproc)) (list (vom::current-node (inner-generator iproc)))))))                        
            iproc))
      (lambda (nproc) (rew num nproc))))

(defun rep (prob max &optional proc)  
  (if proc
      (if (typep proc 'function)
          (lambda (&optional nproc) (rep prob max (funcall proc nproc)))                   
          (let ((iproc (if (symbolp proc) (gethash proc *processor-directory*) proc)))
            (loop for sym in (vom::alphabet (inner-generator iproc))
                  do (let ((next (if (cadr (member sym (vom::alphabet (inner-generator iproc))))
                                     (cadr (member sym (vom::alphabet (inner-generator iproc))))
                                     (car (vom::alphabet (inner-generator iproc))))))
                       (when (and (not (vom::has-transition (inner-generator iproc) (list sym) sym))
                                  (< (random 100) prob))
                         (vom::insert-rule (inner-generator iproc) (list (list sym) sym (* prob 0.01)))
                         ;;(format t "RULE ~D~% " (list (list sym) sym (* prob 0.01)))
                         (vom::insert-rule (inner-generator iproc) (list (make-list max :initial-element sym)
                                                                         next 1.0))
                         ;;(format t "MAX RULE ~D~% " (list (make-list max :initial-element sym) next 1.0))
                         (vom::rebalance-state (inner-generator iproc) (list sym))))))
          iproc)
      (lambda (nproc) (rep prob max nproc))))

(defun sharpen (factor &optional proc)  
  (if proc
      (if (typep proc 'function)
          (lambda (&optional nproc) (sharpen factor (funcall proc nproc)))         
          (progn
            (vom::sharpen-pfa (inner-generator (if (symbolp proc) (gethash proc *processor-directory*) proc)) factor)
            (set-modified proc)))
      (lambda (nproc) (sharpen factor nproc))))

(defun blur (factor &optional proc)  
  (if proc
      (if (typep proc 'function)
          (lambda (&optional nproc) (blur factor (funcall proc nproc)))         
          (progn (vom::blur-pfa (inner-generator (if (symbolp proc) (gethash proc *processor-directory*) proc)) factor)
                 (set-modified proc)))
      (lambda (nproc) (blur factor nproc))))

(defun discourage (factor &optional proc)  
  (if proc
      (if (typep proc 'function)
          (lambda (&optional nproc) (discourage factor (funcall proc nproc)))
          (progn
            (vom::discourage-pfa (inner-generator (if (symbolp proc) (gethash proc *processor-directory*) proc)) factor)
            (set-modified proc)))
      (lambda (nproc) (discourage factor nproc))))

(defun encourage (factor &optional proc)  
  (if proc
      (if (typep proc 'function)
          (lambda (&optional nproc) (encourage factor (funcall proc nproc)))         
          (progn
            (vom::encourage-pfa (inner-generator (if (symbolp proc) (gethash proc *processor-directory*) proc)) factor)
            (set-modified proc)))
      (lambda (nproc) (encourage factor nproc))))

(defun rnd (chance &optional proc)
  (if proc
      (if (typep proc 'function)
          (lambda (&optional nproc) (rnd chance (funcall proc nproc)))         
          (progn
            (vom::randomize-edges (inner-generator (if (symbolp proc) (gethash proc *processor-directory*) proc)) chance)
            (set-modified proc)))
      (lambda (nproc) (rnd chance nproc))))


