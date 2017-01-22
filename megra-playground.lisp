(load "megra-dispatchers")
(load "megra-constructors")

					; define test graph structure
(graph 'uno-midi
       (node 1 (midi 65 :lvl .4 :dur 50))
       (node 2 (midi 81 :lvl 1 :dur 50))
       (edge 1 2 :prob 100 :dur 200)
       (edge 2 1 :prob 100 :dur 200))

(graph 'dos-midi
       (node 1 (midi 67 :lvl .9 :dur 50))
       (node 2 (midi 69 :lvl .4 :dur 50))
       (edge 1 2 :prob 100 :dur 500)
       (edge 2 1 :prob 100 :dur 500))

(graph 'tres-midi
       (node 1 (midi 83 :lvl .9 :dur 50))
       (edge 1 1 :prob 100 :dur 500))



					; init
(incudine:rt-start)
					; stop scheduling
(incudine:rt-stop)

(dispatch 'tres-midi)
(dispatch
 'uno-midi
 'dos-midi)


(deactivate 'tres-midi)
(deactivate 'uno-midi)

