; haven't found a solution to wrap this in a function yet
(defun megra-init ()
  (progn
    (incudine:rt-start)
    (sleep 1)
    (midi-open-default :direction :input)
    (midi-open-default :direction :output)
    (osc-open-default :host "127.0.0.1" :port 3002 :direction :input)
    (osc-open-default :host "127.0.0.1" :port 3003 :direction :output)    
    (setf *out* (new incudine-stream))
    (setf *rts-out* *out*)))

(defun megra-stop ()
  (incudine:rt-stop))
