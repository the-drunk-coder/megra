# Mégra Tutorial, Chapter 2 - Writing Generators

As we've seen in the last chapter, Mégra is all about *sequence generators*. There's 
three principal ways to write *sequence generators* in Mégra:

1. [Writing generators by hand](21-writing-generators-by-hand).
2. [Generating generators](22-generating-generators).
3. [Learing generators](23-learning-generators).

## 2.1 Writing Generators by Hand 
This is the method we've already encountered in the previous chapter. It's a bit tedious, but it's a good way to become familiar with the model. Using short lables, it becomes somewhat manageable:

```lisp
(sx 'boom t
    (infer 'bap
           :events 'b (bd) 's (sn) 'h (hats)
           :rules
           '((b) b 80 400) '((b b b b) h 70 200) '((b b b b) s 30 400)
           '((b) s 10 400) '((b) h 10 200)           
           '((s) b 100 400)           
           '((h) b 100 400)))
```


That's a bit shorter, but still a lot of lines to write before you can hear the first sound. Luckily, there's some shorter methods here ! 

## 2.2 Generating Generators

We can save lots of lines of code by generate the generators from a more abstract description, or we can start out from
a very simple, repeated event and "grow" a generator by repeatedly adding new nodes. Internally, they just generate the long-form
code that we've seen above. The results can even be exported to code.

Let's look at the second method first !

### 2.2.1 Growing Generators

Let's look at a simple generator with one repeating node:

```lisp
(sx 'acid t
    (infer 'bass
           :events 'a (saw 90 :dur 110 :atk 3 :rel 100 :lp-freq 530)
           :rules '((a) a 100 120)))
```

Can we make this a little shorter ? Yes, we can ! The one above is the same as this one:

```lisp
(sx 'acid t
  (nuc 'bass (saw 90 :dur 110 :atk 3 :rel 100 :lp-freq 330) :dur 120)) 
```

The `(nuc ...)` function is short for *Nucleus*, if you were wondering.

Now behold (or, better, execute the following several times):

```lisp
(grow 'bass :var 0.8 :method 'loop) ;; <- play with the variance a bit, but stay below 1.0
```

Check what you have just cultivated:

```lisp
(to-svg 'bass) ;; <- all graphviz renderers available, circo or dot most common
```

This will generate an SVG (if you don't provide an absolute path, it'll be in the
Portacle root folder).

You can also turn the result back into code by running the following code that'll
print the code to the command prompt:

```lisp
(to-code 'bass t) ;; to repl 
```

If you want to export to a file, just provide a filename:

```lisp
(to-code 'bass "bass") ;; to file
```

If you want to be able to load the result directly from the file, make it loadable:

```lisp
(to-code 'bass "bass" :loadable t) ;; to file, loadable ...
```
Now, you can load the result directly and fetch it via `(getgen)`:

```lisp
(load "bass.megra")

(sx 'ga t
    (getgen 'bass))
```

### 2.2.2 Creating Generators from an Abstract Description

Mégra contains a sub-language to describe loops (that are called *Cycles* here).

That way, a simple, more-or-less cyclic event generator can be created with just 
two lines of code:

```lisp
(sx 'a-basic t
  (cyc 'beat "bd ~ ~ ~ sn ~ ~ ~" :dur 100)) ;; roughly 150 bpm ... ~ is silence
```

This loop contains eight events, of which 6 are just a silent placeholder. It could be written like this:

```lisp
(sx 'a-basic t
  (cyc 'beat "bd sn" :dur 400)) ;; note the longer duration!
```

I prefer the first method because it allows for easier modification ... like, how about we add some hi-hats ?

```lisp
(sx 'a-basic t
  (cyc 'beat "[hats bd] ~ hats ~ [sn hats] ~ hats ~" :dur 100)) 
```

If this reminds you a bit of TidalCycles, you're right, but note that the square bracket syntax is different. It just means "play at the same time". Anyway, the thing above would be harder to do if we just had the two events, because there'd be no space for our hi-hats and we'd need to re-write the whole thing.

As you can see above, the events are evenly spaced over the course of the cycle, where the duration between each event is the same.

If you want more time control, you can define explicit transition times. A number in the cycle description is interpreted as an explicit transition time! The beat below contains two double strokes, one on the hi-hat, one on the snare:

```lisp
(sx 'a-basic t
  (cyc 'beat "[hats bd] ~ hats 50 hats 50 ~ [sn hats] 50 sn 50 ~ hats ~" :dur 100)) 
```

If that's all a bit too cyclical for you, how about we allow for the repetition of events ? 

```lisp
(sx 'a-basic t
  (cyc 'beat "[hats bd] ~ hats 50 hats 50 ~ [sn hats] 50 sn 50 ~ hats ~" :dur 100 :rep 80 :max-rep 4)) 
```

The `:rep 80` parameter says "for every node, there's a chance of 80 percent that it'll be repeated. The `:max-rep 4` parameter specifies that the maximum number of repetitions will be 4. Internally, it'll add rules like:

```lisp
'((1) 1 80 100) ;; <- the 'rep' parameter will generate this
'((1 1 1 1) 2 100 100) ;; <- the 'max-rep' parameter will generate this 
```

If that's still not enough variation for you, you can also add a chance that random connections between nodes will be created:

```lisp
(sx 'a-basic t
  (cyc 'beat "[hats bd] ~ hats 50 hats 50 ~ [sn hats] 50 sn 50 ~ hats ~" :dur 100 :rep 80 :max-rep 4 :rnd 70)) 
```

Now there's a 70% chance that random connections between nodes will be created, specified by the `:rnd 70` parameter!

And finally, you can grow the beat if you wish:

```lisp
(grow 'beat :var 0.8 :method 'triloop)
```

Everything beyong that would probably be just randomness ...

## 2.3 Learning Generators

Now we're arriving in 2020, where machine learning is all the rage, and it's about time time to introduce a variant of the underlying model. So far, we've worked with a fairly naive implementation of what is called "Probablistic Finite Automata".

To *learn* sequence generators from data, we need a second implementation is closer to the mathematical theory and allows to use simple machine learning techniques to generate generators.

Let's start by learning a generator from a user-provided distribution by running the followign code:

```lisp
(sx 'learned t
  (learn 'distr ;; 
          :events (x (sn) o (bd) - (hats)) ;; <- First, define an event key
          "x---o---x---o---x---o---x---o---" ;; <- Now, provide a distribution with the defined events
          :dur 300)) 
```

You might notice that this is very different from the results of the `cyc` generator. Even if the provided string looks very much 
like a cycle, it doesn't mean that we can expect an exact cycle when learning from such a small dataset in such a short amount of time.
But, on the other hand, we can use this in real time and still get inspiring results. That's the magic of **Small Data**.

Let's see what happens when we modify the distribution ...

```lisp
(sx 'learned t
  (slearn 'distr 
          :events 'x (sn) 'o (bd) '- (hats) ;; <- First, define an event key, as before for infer
          "x---o---xoooxoxooxooxoxoxoxoxoxoxo-xoxoxoxooooxxxxoooxoxoxo--o---x---oxoxoxoxoxoxooooxxxxoo---x---o---"))
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
	 :type 'pfa ;; <- just set the type flag, 'naive being the other one
         :events 'a (saw 100) 'b (saw 50) 'c (saw 200)
	 :rules '((a) b 0.5)   
	 	'((a) a 0.5)
	        '((b) a 1.0)
		'((a b a b) c 1.0)
		'((c) a 1.0))
	 :dur 300))
```
They react a bit different and sometimes are a bit less predictable, but can give interesting results.

Finally, there's nuc and cyc constructors as well for the "new" model.

```lisp
(s 'faum ()  
  (cyc2 'lopa "bd 200 hats 200 [tri:600 sn] 200 hats 200 bd 200 hats 200 sn" :dur 400))

(s 'daum (:sync 'faum)
  (always (filter-lp 1000) (dur 190) (atk 2) (rel 180) (lvl 0.2))
  (nuc2 'pola (saw 100) :dur 200))
```


