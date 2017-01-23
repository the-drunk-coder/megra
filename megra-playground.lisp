(load "megra-dispatchers")
(load "megra-constructors")

(in-package :cm)

(init-megra)

(in-package :common-lisp-user)

					; init scheduling
(incudine:rt-start)
					; stop scheduling
(incudine:rt-stop)

					; define test graph structures
(graph 'uno-midi
       (node 1 (midi 65 :lvl .4 :dur 50))
       (node 2 (midi 81 :lvl 1 :dur 50))
       (edge 1 2 :prob 100 :dur 200)
       (edge 2 1 :prob 100 :dur 200))

(graph 'dos-midi
       (node 1 (midi 67 :lvl .2 :dur 350))
       (node 2 (midi 73 :lvl .9 :dur 350))
       (edge 1 2 :prob 100 :dur 500)
       (edge 2 1 :prob 100 :dur 500))

(graph 'tres-midi
       (node 1 (midi 83 :lvl .9 :dur 150))
       (edge 1 1 :prob 100 :dur 520))

(dispatch 'tres-midi)

; tree-like dispatch branching ?

(dispatch
 'uno-midi
					; start value is optional, if not specified,
					; original event stuff is taken
 (oscillate-between 'o2 'lvl 0.1 0.5 :start 0.4) 
 'dos-midi)

(deactivate 'tres-midi)

(deactivate 'uno-midi)

