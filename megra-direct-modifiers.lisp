(in-package :megra)

;; FUNCTIONS that modify the PFAs or their evaluations order ...

;; SKIP
(defun skip (num &optional proc)    
  (if proc
      (if (typep proc 'function)
          (lambda (nproc) (skip num (funcall proc nproc)))
          (progn
            (loop for a from 0 to (- num 1)
                  do (progn                              
                       (pull-events proc :skip-successor t)
                       (pull-transition proc :skip-successor t)))
            proc))
      (lambda (nproc) (skip num nproc))))

;; GROWN
(defun grown (n var method &optional proc)  
  (if proc      
      (if (typep proc 'function)
          (lambda (nproc) (grown n var method (funcall proc nproc)))
          (progn (loop for a from 0 to n
                       do (grow proc :var var :method method))
                 proc))
      (lambda (nproc) (grown n var method nproc))))

(defun grown2 (n var method &optional proc)  
  (if proc
      (if (typep proc 'function)
          (lambda (nproc) (grown2 n var method (funcall proc nproc)))
          (progn (loop for a from 0 to n
                       do (grow2 proc :var var :method method))
                 proc))
      (lambda (nproc) (grown2 n var method nproc))))

;; haste 4 0.5 - apply tempo mod for the next n times (only on base proc)
(defun haste (num mod &optional proc)  
  (if proc
      (if (typep proc 'function)
          (lambda (nproc) (haste num mod (funcall proc nproc)))
          (progn (loop for a from 0 to (- num 1)
                       do (push-tmod proc mod))
                 proc))
      (lambda (nproc) (haste num mod nproc))))

;; relax 4 0.5 - apply tempo mod for the next n times (only on base proc)
(defun relax (num mod &optional proc)  
  (if proc
      (if (typep proc 'function)
          (lambda (nproc) (relax num mod (funcall proc nproc)))
          (progn (loop for a from 0 to (- num 1)
                       do (push-tmod proc (coerce (/ 1.0 mod) 'float)))
                 proc))
      (lambda (nproc) (relax num mod nproc))))

;; rew 3 - rewind (set to state n back in traced path)
(defun rew (num &optional proc)  
  (if proc
      (if (typep proc 'function)
          (lambda (nproc) (rew num (funcall proc nproc)))
          (progn            
            (set-current-node proc (list (nth (- (trace-length proc) (+ num 1)) (traced-path proc))))
            (set-traced-path proc(append (traced-path proc) (current-node proc))) 
            (when (> (list-length (traced-path proc)) (trace-length proc))
              (set-traced-path proc
	                       (delete (car (traced-path proc)) (traced-path proc) :count 1)))
            proc)))
  (lambda (nproc) (rew num nproc)))


;; needs traced path for pfa and state setter method, ideally for both ... 

