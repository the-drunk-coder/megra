;; required packages
(require 'cm)
;; first start incudine (otherwise the read-macros are not working ...)
(in-package :cm)
(progn
    (incudine:rt-start)
    (sleep 1)
    (midi-open-default :direction :input)
    (midi-open-default :direction :output)
    (osc-open-default :host "127.0.0.1" :port 3002 :direction :input)
    (osc-open-default :host "127.0.0.1" :port 3003 :direction :output)    
    (setf *out* (new incudine-stream))
    (setf *rts-out* *out*))

;; then load the megra dsp stuff .. wait until compilation has finished !!
(load "megra-dsp")

(load "megra-package")

(in-package :megra)

;; define some test graph structures
(graph 'uno-midi
       (node 1 (mid 65 :lvl .4 :dur 50))
       (node 2 (mid 81 :lvl 1 :dur 50) (mid 50 :lvl 1 :dur 50))
       (edge 1 2 :prob 100 :dur 200)
       (edge 2 1 :prob 100 :dur 200))

;; individual graphs are basically first-order markov chains ...
(graph 'dos-midi
       (node 1 (mid 59 :lvl .8 :dur 350))
       (node 2 (mid 73 :lvl .9 :dur 350))
       (node 3 (mid 78 :lvl .9 :dur 350))
       (edge 1 2 :prob 50 :dur 500)
       (edge 1 3 :prob 50 :dur 750)
       (edge 2 1 :prob 100 :dur 500)
       (edge 3 1 :prob 100 :dur 250))

(graph 'tres-midi
       (node 1 (mid 84 :lvl .9 :dur 150))
       (edge 1 1 :prob 100 :dur 100))

(graph 'the-grain
       (node 1 (grain "misc" "tada" :dur 128 :lvl 1.0 :rate 0.5))
       (edge 1 1 :prob 100 :dur 32))


(dispatch
 (oscillate-between 'rate-o 'rate 0.2 1.3 :cycle 40) 
 'the-grain)

(deactivate 'rate-o)


;; dispatch a graph to make it sound 
(dispatch
  'uno-midi)

(deactivate 'uno-midi)

(dispatch
  'dos-midi)

;; PERMANENT CHANGE
;; this should be passed as parameter to (graph ... )
;; so i might have to replace (graph ..) by a macro ??
(setf (copy-events (gethash 'tres-midi *processor-directory*)) nil)

(dispatch
 (brownian-motion 'tres-rw 'pitch :step 2 :ubound 84 :lbound 50 :wrap t :track-state nil)
 (oscillate-between 'o2 'lvl 0.0 1.0 :cycle 200) 
 'tres-midi)

;; TRANSITORY STATE (default)
(dispatch
 (brownian-motion 'tres-rw 'pitch :step 4 :ubound 84 :lbound 50 :wrap t)
 (oscillate-between 'o2 'lvl 0.0 1.0 :cycle 100) 
 'tres-midi)

(deactivate 'tres-rw)

(deactivate 'dos-midi)

;; the last graph in the chain determines the timing, so each
;; processor chain needs a unique ending point, but it's possible
;; for multiple processors to have the same predecessor ... 
(dispatch 
 'uno-midi
 'tres-midi) 

;; deactivating the first processor in a chain makes it stop ...
;; if it's a modifier, the modifier needs to be deactivated;; as everything is named, this shouldn't pose a problem ... 
(deactivate 'uno-midi)
(deactivate 'dos-midi)
(deactivate 'tres-midi)

;; hook an event modifier into the chain ...
(dispatch
 'tres-midi
 (brownian-motion 'tres-rw 'pitch :step 5 :ubound 84 :lbound 50 :wrap t)
 'uno-midi)

;; TBD:
;; eventually make multiple dispatching possible ... like, (dispatch :check-active nil ...)
;; arranging modifiers in graphs ...
;; define meaningful behaviour for non-mandatory modifiers ...
;; (chance ...) shortcut ... even though the semantics of "chance" is
;;    slightly different, as is evaluates the chance of something to happen for the current
;;    event, whereas a graph modifies the modification for the next event ... 
;; graph-theory stuff -- graph transformations etc
;; syncstart
;; midi note blocker for disklavier
;; vugs
;; get rid of deactivating error msg ...

;; DONE:
;; (brownian-motion 'tres-rw 'pitch :step 4 :ubound 84 :lbound 50 :wrap t TRACK-STATE: nil) -- makes sense in comination with below ... ??
;; (graph 'xyz :copy-events nil) --- original events are sent out, makes sense in comination with the above
;; oscillating event modifier -- works !
;; tree-like dispatch branching -- works !
;; avoid duplicate dispatches -- works !
;; automatic re-activation -- works !





