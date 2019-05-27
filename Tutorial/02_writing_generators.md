# Mégra Tutorial, Chapter 2 - Writing Generators

As we've seen in the last chapter, Mégra is all about *sequence generators*. There's 
three principal ways to write *sequence generators* in Mégra:

1. Writing generators by hand.
2. Generating generators.
3. Learing generators.

## 2.1 Writing Generators by Hand 
This is the method we've already encountered in the previous chapter. It's a bit tedious, but it's
a good way to become familiar with the model. There's also some shorthands. Let's look again at
the beat generator from the last chapter:

```lisp
(sink 'boom () ;; <- this turns the events into actual sounds. More about that later! 
	(graph 'bap () ;; <- this creates the generator
		(node 1 (bd)) ;; <- each node has a number and a list of associated events 
		(node 2 (sn))
		(node 3 (hats))
		(edge 1 1 :prob 80 :dur 400) 		
		(edge 1 2 :prob 10 :dur 400) 
		(edge 1 3 :prob 10 :dur 200) 
		(edge '(1 1 1 1) 3 :prob 70 :dur 200) 
		(edge '(1 1 1 1) 2 :prob 30 :dur 200) 
		(edge 2 1 :prob 100 :dur 400)
		(edge 3 1 :prob 100 :dur 400)))
```

This can be shortened to:

```lisp
(s 'boom () ;; sink becomes s
	(g 'bap ()  ;; graph becomes g
		(n 1 (bd)) ;; node becomes n
		(n 2 (sn))
		(n 3 (hats))
		(e 1 1 :p 80 :d 400) ;; edge becomes e, prob becomes p, dur becomes d
		(e 1 2 :p 10 :d 400) 
		(e 1 3 :p 10 :d 200) 
		(e '(1 1 1 1) 3 :p 70 :d 200) 
		(e '(1 1 1 1) 2 :p 30 :d 200) 
		(e 2 1 :p 100 :d 400)
		(e 3 1 :p 100 :d 400)))
```

That's a bit shorter, but still a lot of lines to write before you can hear the first sound. Luckily, there's 
some shorter methods here ! 

## 2.2 Generating Generators

We can save lots of lines of code by generate the generators from a more abstract description, or we can start out from
a very simple, repeated event and "grow" a generator by repeatedly adding new nodes. Internally, they just generate the long-form
code that we've seen above. The results can even be exported to code.

Let's look at the second method first !

### 2.2.1 Growing Generators

Let's look at a simple generator with one repeating node:

```lisp
(s 'acid ()
  (g 'bass ()
    (n 1 (saw 90 :dur 110 :atk 3 :rel 100 :lp-freq 530))
    (e 1 1 :d 120 :p 100)))
```
Can we make this a little shorter ? Yes, we can ! The one above is the same as this one:

```lisp
(s 'acid ()
  (nuc 'bass (saw 90 :dur 110 :atk 3 :rel 100 :lp-freq 330) :dur 120)) 
```

The `(nuc ...)` function is short for *Nucleus*, if you were wondering.

Now behold (or, better, execute the following several times):

```lisp
(grow 'bass :var 0.8 :method 'loop) ;; <- play with the variance a bit, but stay below 1.0
```

Check what you have just cultivated:

```lisp
(graph->svg 'bass "bass-diagram" :renderer 'neato) ;; <- all graphviz renderers avai
```

This will generate an SVG (if you don't provide an absolute path, it'll be in the
Portacle root folder).

You can also turn the result back into code by running the following code that'll
print the code to the command prompt:

```lisp
(pring 'bass t) ;; pring is short for 'print graph' 
```
### 2.2.2 Creating Generators from an Abstract Description

Mégra contains a sub-language to describe loops (that are called *Cycles* here).

That way, a simple, more-or-less cyclic event generator can be created with just 
two lines of code:

```lisp
(s 'a-basic ()
  (cyc 'beat "bd ~ ~ ~ sn ~ ~ ~" :dur 100)) ;; roughly 150 bpm ... ~ is silence
```

This loop contains eight events, of which 6 are just a silent placeholder. It could be written like this:

```lisp
(s 'a-basic ()
  (cyc 'beat "bd sn" :dur 400)) ;; note the longer duration!
```

I prefer the first method because it allows for easier modification ... like, how about we add some hi-hats ?

```lisp
(s 'a-basic ()
  (cyc 'beat "[hats bd] ~ hats ~ [sn hats] ~ hats ~" :dur 100)) 
```

If this reminds you a bit of TidalCycles, you're right, but note that the square bracket syntax is different. It just means "play at the same time". Anyway, the thing above would be harder to do if we just had the two events, because there'd be no space for our hi-hats and we'd need to re-write the whole thing.

As you can see above, the events are evenly spaced over the course of the cycle, where the duration between each event is the same.

If you want more time control, you can define explicit transition times. A number in the cycle description is interpreted as an explicit transition time! The beat below contains two double strokes, one on the hi-hat, one on the snare:

```lisp
(s 'a-basic ()
  (cyc 'beat "[hats bd] ~ hats 50 hats 50 ~ [sn hats] 50 sn 50 ~ hats ~" :dur 100)) 
```

If that's all a bit too cyclical for you, how about we allow for the repetition of events ? 

```lisp
(s 'a-basic ()
  (cyc 'beat "[hats bd] ~ hats 50 hats 50 ~ [sn hats] 50 sn 50 ~ hats ~" :dur 100 :rep 80 :max-rep 4)) 
```

The `:rep 80` parameter says "for every node, there's a chance of 80 percent that it'll be repeated. The `:max-rep 4` parameter specifies that the maximum number of repetitions will be 4. Internally, it'll add rules like:

```lisp
(e 1 1 :p 80 :d 100) ;; <- the 'rep' parameter will generate this
(e '(1 1 1 1) 2 :p 100 :d 100) ;; <- the 'max-rep' parameter will generate this 
```

If that's still not enough variation for you, you can also add a chance that random connections between nodes will be created:

```lisp
(s 'a-basic ()
  (cyc 'beat "[hats bd] ~ hats 50 hats 50 ~ [sn hats] 50 sn 50 ~ hats ~" :dur 100 :rep 80 :max-rep 4 :rnd 70)) 
```

Now there's a 70% chance that random connections between nodes will be created, specified by the `:rnd 70` parameter!

And finally, you can grow the beat if you wish:

```lisp
(grow 'beat :var 0.8 :method 'triloop)
```

Everything beyong that would probably be just randomness ...

## 2.3 Learning Generators

Now we're arriving in 2019, where machine learning is all the rage, and it's about time time to introduce a variant of the underlying model. So far, we've worked with a fairly naive implementation of what is called "Probablistic Finite Automata".

To *learn* sequence generators from data, we need a second implementation is closer to the mathematical theory and allows to use simple machine learning techniques to generate generators.

Unfortunately at this stage of development, the two models are not compatible, so the `grow` function doesn't work on generators generated by this model. Anyway, let's ignore those technical details for now.

Let's start by learning a generator from a user-provided distribution by running the followign code:

```lisp
(s 'learned ()
  (slearn 'distr ;; <- slearn stands for "shorthand learn"
          (x (sn) o (bd) - (hats)) ;; <- First, define an event key
          "x---o---x---o---x---o---x---o---" ;; <- Now, provide a distribution with the defined events
          :dur 300)) 
```

You might notice that this is very different from the results of the `cyc` generator. Even if the provided string looks very much 
like a cycle, it doesn't mean that we can expect an exact cycle when learning from such a small dataset in such a short amount of time.
But, on the other hand, we can use this in real time and still get inspiring results. That's the magic of **Small Data**.

Let's see what happens when we modify the distribution ...

```lisp
(s 'learned ()
  (slearn 'distr 
          (x (sn) o (bd) - (hats)) ;; <- First, define an event key
          "x---o---xoooxoxooxooxoxoxoxoxoxoxo-xoxoxoxooooxxxxoooxoxoxo--o---x---oxoxoxoxoxoxooooxxxxoo---x---o---"
          :dur 300))
```

While both sound very different, do they sound "as expected" ? Guess that's up to you to decide,
but note that those are not "pattern", what you see is NOT what you hear ! 

For the sake of completeness, there's also a way to specify a generator based on the "new" model by rules:	  

```lisp
;; this is closer to the old graph model, you define rules by hand !
;; the "missing" rules are inferred  
(s 'grod (:sync 'learned)
  (always (dur 50) (atk 1) (rel 40) (filter-lp (oscil 900 1900))) ;; set params for saw bass  
  (infer 'glob       
	 (events (a (saw 100)) (b (saw 50)) (c (saw 200)))
	 (rules ((a) b 0.5)
		((a) a 0.5)
		((b) a 1.0)
		((a b a b) c 1.0)
		((c) a 1.0))
	 :dur 300))
```
Did i say the `grow` function doesn't work on generators created that way ? Well, i guess that wasn't the whole truth. There's the `grow2` function ... try these: 

```lisp
(grow2 'distr :var 0.9)
(grow2 'glop :var 0.9)
```
They react a bit different and are a bit less predictable, but can give interesting results.

Finally, there's nuc and cyc constructors as well for the "new" model.

```lisp
(s 'faum ()  
  (cyc2 'lopa "bd 200 hats 200 [tri:600 sn] 200 hats 200 bd 200 hats 200 sn" :dur 400))

(s 'daum (:sync 'faum)
  (always (filter-lp 1000) (dur 190) (atk 2) (rel 180) (lvl 0.2))
  (nuc2 'pola (saw 100) :dur 200))
```


