(require 'cm)
(in-package :cm)

;; initialize -- seems like it has to be like this ...
(progn
  (incudine:set-rt-block-size 128)
  (incudine:rt-start)
  (sleep 1)
  ;;(midi-open-default :direction :input)
  (defvar *midiin* (midi-open-default :direction :input))
  (defvar *midiout* (midi-open-default :direction :output))
  (osc-open-default :host "127.0.0.1" :port 3002 :direction :input)
  ;; send osc msgs to scsynth ...
  (osc-open-default :host "127.0.0.1" :port 57110 :direction :output)    
  (setf *out* (cm::new cm::incudine-stream))
  (setf *rts-out* *out*)
  ;;(setf (incudine::logger-level) :info)
  (incudine::recv-start *midiin*))

(incudine:rt-stop)

;; then load the megra dsp stuff .. wait until compilation has finished !!
(compile-file "megra-dsp-atoms")

(load "megra-dsp-atoms")

;; wait !
(compile-file "megra-dsp")

(load "megra-dsp")

;; now everything should be ready to load the megra package ... 
(load "megra-package")

(in-package :megra)

(ql:update-all-dists)

(defparameter *default-dsp-backend* 'inc)
;;(defparameter *default-dsp-backend* 'sc)

