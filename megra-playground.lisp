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

;; define some test graph structures
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


;; dispatch a graph to make it sound 
(dispatch
  'uno-midi)

(dispatch
  'dos-midi)

(dispatch
  'tres-midi)

;; the last graph in the chain determines the timing, so each
;; processor chain needs a unique ending point, but it's possible
;; for multiple processors to have the same predecessor ... 
(dispatch 
 'uno-midi
 'tres-midi) 

;; deactivating the first processor in a chain makes it stop ...
;; if it's a modifier, the modifier needs to be deactivated
;; as everything is named, this shouldn't pose a problem ... 
(deactivate 'uno-midi)
(deactivate 'dos-midi)
(deactivate 'tres-midi)

;; hook an event modifier into the chain ...
(dispatch
 'tres-midi
 (brownian-motion 'tres-rw 'pitch :step 5 :ubound 84 :lbound 50 :wrap t)
 'uno-midi)

(oscillate-between 'o2 'lvl 0.1 0.5) 


;; TBD:
;; oscillating event modifier
;; syncstart
;; midi note blocker for disklavier

;; DONE:
;; tree-like dispatch branching -- works !
;; avoid duplicate dispatches -- works !
;; automatic re-activation -- works !
