# Mégra Tutorial, Chapter 4 - Sync and Shift

## Synchronizing

So far, we've only worked with a single structure. While this might bring us quite far, at some point you might 
want to work with more than just one. 

```lisp
(s 'beat ()  
  (cyc 'one "bd:'boom sn:'tschack" :dur 400))

(s 'hats ()  
  (cyc 'three "hats hats hats hats" :dur 200))

(s 'bass ()  
  (nuc 'two (saw 90 :lp-dist 0.5 :lp-freq 1000 :atk 1 :rel 99 :dur 100 :lvl 0.5)                  
       :dur 100))      
```

If you run those one by one, you might have a hard time synchronizing them. 

You could try to run them at the same time using the `(progn ...)` function, a classic lisp function to execute functions 
in succession, which in our case means *practically at the same time* (even though it's not technically the case).

```lisp
(progn
  (s 'beat ()  
    (cyc 'one "bd:'laid sn:'dub" :dur 400))  
  (s 'hats ()  
    (cyc 'three "hats hats hats hats" :dur 200))  
  (s 'bass ()  
    (nuc 'two (saw 90 :lp-dist 0.5 :lp-freq 1000 :atk 1 :rel 99 :dur 100 :lvl 0.5)                  
         :dur 100)))
```

This is helpful, but if you execute it multiple times, you might notice it's not super-reliable. Also, what if you don't want to 
execute everything at the same time ? that's what the `sync` parameter is for:

```lisp
(s 'beat ()  
  (cyc 'one "bd:'boom sn:'tschack" :dur 400))

(s 'hats (:sync 'beat) ;; <- sync to beat ... 
  (cyc 'three "hats hats hats hats" :dur 200))

(s 'bass (:sync 'beat) ;; <- also sync to beat  
  (nuc 'two (saw 90 :lp-dist 0.5 :lp-freq 1000 :atk 1 :rel 99 :dur 100 :lvl 0.5)                  
       :dur 100))      
```

That way, the `hats` and `bass` sinks will wait until `beat` emits its next event. That way, the generators can be synchronized. 
Note that this only synchronizes for one point in time.

# Shifting

Sometimes you might want to give your beat a bit of a swing or shuffle, or create an offbeat. First, look at the following:

```lisp
(s 'beat ()  
  (cyc 'one "bd sn" :dur 600))

(s 'hats (:sync 'beat)  
  (cyc 'three "hats hats hats hats" :dur 600))
```

Now, to shift the hi-hat around, you can use the `shift` parameter:

```lisp
(s 'beat ()  
  (cyc 'one "bd sn" :dur 600))

(s 'hats (:sync 'beat :shift 300) ;; <- use shift to create an offbeat 
  (cyc 'three "hats hats hats hats" :dur 600))
```

Play around with the shift parameter a bit! You might discover a way to create a swing beat:

```lisp
(s 'beat ()  
  (cyc 'one "bd sn" :dur 600))

(s 'hats (:sync 'beat :shift 300) ;; <- use shift to create a shuffle feel
  (cyc 'three "hats hats hats hats" :dur 600))
```

Now you should have the ability to distribute your musical material over multiple generators.



