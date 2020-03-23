(in-package :megra)

;; compose function allows for convenient application
;; of higher-order functions ...
(defun cmp (&rest rest)
  (if (cdr rest)
      (let ((rev (reverse rest)))
        (labels ((accum (acc r)
                   (if r
                       (accum (funcall (car r) acc) (cdr r))
                       acc)))
          (accum (funcall (cadr rev) (car rev)) (cddr rev))))      
      (car rest)))





