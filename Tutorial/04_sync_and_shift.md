# Mégra Tutorial, Chapter 4 - Sync and Shift

## Synchronizing

So far, we've only worked with a single structure. While this might bring us quite far, at some point you might 
want to work with more than just one. 

```lisp
(sx 'beat t
  (cyc 'one "bd:'boom sn:'tschack" :dur 400))

(sx 'hats t  
  (cyc 'three "hats hats hats hats" :dur 200))

(sx 'bass t
  (nuc 'two (saw 90 :lp-dist 0.5 :lp-freq 1000 :atk 1 :rel 99 :dur 100 :lvl 0.5)                  
       :dur 100))      
```

If you run those one by one, you might have a hard time synchronizing them. 

The easiest way to synchronize them is to group them to a single sink

```lisp
(sx 'beat-hats-bass t  
    (nuc 'two (saw 90 :lp-dist 0.5 :lp-freq 1000 :atk 1 :rel 99 :dur 100 :lvl 0.5)                  
       :dur 100)
    (cyc 'three "hats hats hats hats" :dur 200)
    (cyc 'one "bd:'boom sn:'tschack" :dur 400))
```

That'll ensure the three generators are in sync. If you want to synchronize another generator (or group of generators)
to this one, that's fairly easy as well:

```lisp
(sx 'treble t :sync 'beat-hats-bass ;; <- just set the sync flag ...
    (nuc 'waves1 (saw 360 :lp-dist 0.5 :lp-freq 1000 :atk 1 :rel 99 :dur 100 :lvl 0.5) :dur 100)
    (nuc 'waves2 (saw 360 :lp-dist 0.5 :lp-freq 1000 :atk 1 :rel 99 :dur 100 :lvl 0.5) :dur 150))
```


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



