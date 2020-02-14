(in-package :megra)

;; compose function allows for convenient application
;; of higher-order functions ...
(defun cmp (&rest rest)
  (labels ((cmp-inner (acc rest)
             (if rest
                 (cmp-inner (funcall acc (car rest)) (cdr rest))
                 acc)))
    (if rest (cmp-inner (car rest) (cdr rest)))))

(cmp
 (pear (dur 200))
 (evr 30 (haste 3 0.5))
 (pprob 40 (relax 2 0.3))
 (grown 20 0.4 'loop)
 (cyc 'a "bd ~ sn ~"))


(s 'a t nil nil
  (xdup
   (pear (rate-mul 2.0))
   (cmp (evr 20 (haste 20.5))
        (grown 20 0.4 'loop)
        (cyc "bd sn")))
  (tie (for bd (always (dur 300))) ;; use old chain macro ??
       (cyc "rate:0.4 ~ rate:0.3")
       (cyc "bd sn")))







