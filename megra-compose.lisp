(in-package :megra)

;; compose function allows for convenient application
;; of higher-order functions ...
(defun cmp (&rest rest)
  (labels ((cmp-inner (acc rest)
             (if rest
                 (cmp-inner (funcall acc (car rest)) (cdr rest))
                 acc)))
    (if rest (cmp-inner (car rest) (cdr rest)))))






