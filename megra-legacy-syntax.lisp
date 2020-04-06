(in-package :megra)

;; a few functions to ensure some compatibility with older megra code ..

(defmacro dispatch-old (name (&key (sync nil) (branch nil) (shift 0.0) (intro nil)) &body proc-body)
  `(funcall (lambda ()
              (sx ',name t :sync ',sync :shift ,shift :intro ,intro
                  (cmp ,@proc-body)))))

;; "sink" alias for "dispatch" ... shorter and maybe more intuitive ... 
(setf (macro-function 'sink) (macro-function 'dispatch-old))

;; even shorter, tidal style ... 
(setf (macro-function 's) (macro-function 'dispatch-old))
