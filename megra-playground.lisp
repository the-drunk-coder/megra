(load "megra-package")

(in-package :cm)

; haven't found a solution to wrap this in a function yet
(progn
  (incudine:rt-start)
  (sleep 1)
  (midi-open-default :direction :input)
  (midi-open-default :direction :output)
  (osc-open-default :host "127.0.0.1" :port 3002 :direction :input)
  (osc-open-default :host "127.0.0.1" :port 3003 :direction :output)
  (fudi-open-default :host "127.0.0.1" :port 3011 :direction :input)
  (fudi-open-default :host "127.0.0.1" :port 3012 :direction :output)
  (setf *out* (new incudine-stream))
  (setf *rts-out* *out*))


(in-package :megra)



					; define test graph structures
(graph 'uno-midi
       (node 1 (mid 65 :lvl .4 :dur 50))
       (node 2 (mid 81 :lvl 1 :dur 50))
       (edge 1 2 :prob 100 :dur 200)
       (edge 2 1 :prob 100 :dur 200))

(graph 'dos-midi
       (node 1 (mid 67 :lvl .2 :dur 350))
       (node 2 (mid 73 :lvl .9 :dur 350))
       (edge 1 2 :prob 100 :dur 500)
       (edge 2 1 :prob 100 :dur 500))

(graph 'tres-midi
       (node 1 (mid 83 :lvl .9 :dur 150))
       (edge 1 1 :prob 100 :dur 220))



(dispatch
 (brownian-motion 'rw-1 'pitch :step 1 :ubound 84 :lbound 30 :wrap t)
 'tres-midi)

; tree-like dispatch branching ?

(dispatch
 'uno-midi
 'dos-midi)

					; start value is optional, if not specified,
					; original event stuff is taken
 (oscillate-between 'o2 'lvl 0.1 0.5 :start 0.4) 

(deactivate 'rw-1)

(deactivate 'uno-midi)

