(require 'cm)


;; haven't found a solution to wrap this in a function yet
(defun megra-init ()
  (progn
    (incudine:rt-start)
    (sleep 1)
    (cm::midi-open-default :direction :input)
    (cm::midi-open-default :direction :output)
    (cm::osc-open-default :host "127.0.0.1" :port 3002 :direction :input)
    (cm::osc-open-default :host "127.0.0.1" :port 3003 :direction :output)    
    (setf *out* (cm::new cm::incudine-stream))
    (setf *rts-out* *out*)))

(defun megra-stop ()
  (incudine:rt-stop))
