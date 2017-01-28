(load "megra-package")

;; init sound/midi processing
(progn
    (incudine:rt-start)
    (sleep 1)
    (midi-open-default :direction :input)
    (midi-open-default :direction :output)
    (osc-open-default :host "127.0.0.1" :port 3002 :direction :input)
    (osc-open-default :host "127.0.0.1" :port 3003 :direction :output)    
    (setf *out* (new incudine-stream))
    (setf *rts-out* *out*))

(in-package :megra)

;; define test graph structures
(graph 'uno-midi
       (node 1 (mid 65 :lvl .4 :dur 50))
       (node 2 (mid 81 :lvl 1 :dur 50) (mid 50 :lvl 1 :dur 50))
       (edge 1 2 :prob 100 :dur 200)
       (edge 2 1 :prob 100 :dur 200))

(graph 'dos-midi
       (node 1 (mid 67 :lvl .2 :dur 350))
       (node 2 (mid 73 :lvl .9 :dur 350))
       (edge 1 2 :prob 100 :dur 500)
       (edge 2 1 :prob 100 :dur 500))

(graph 'tres-midi
       (node 1 (mid 84 :lvl .9 :dur 150))
       (edge 1 1 :prob 100 :dur 1000))

(graph 'quatr-midi
       (node 1 (mid 82 :lvl .9 :dur 150))
       (edge 1 1 :prob 100 :dur 120))

(dispatch 
 'uno-midi
 (brownian-motion 'tres-rw 'pitch :step 5 :ubound 84 :lbound 50 :wrap t)
 'tres-midi)

(dispatch
 'tres-midi
 'uno-midi)

(dispatch 
 'tres-midi)

(dispatch
  'tres-midi)

(deactivate 'uno-midi)

(deactivate 'tres-midi)

(dispatch
 'quatr-midi)

(is-active (gethash 'quatr-midi *processor-directory*))

; tree-like dispatch branching ?

					; -- tbd
					; syncstart
					; avoid duplicate dispatches
					; midi note blocker
					; automatic re-activation
(dispatch
 'uno-midi
 'dos-midi)

					; start value is optional, if not specified,
					; original event stuff is taken
 (oscillate-between 'o2 'lvl 0.1 0.5 :start 0.4) 

(deactivate 'rw-2)



