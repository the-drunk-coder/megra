# Mégra Tutorial, Chapter 3 - Controlling Sound

In the last chapters, the main topic was creating generators for musical structures. Now we will look into how to 
control sound parameters with Mégra.

## Sample Sounds

### Choosing Samples

In the last chapter, we created beats from samples. You might have noticed that the sound changes sometimes when you re-run the code. Before we get deeper into that, first take a look into the `megra-samples` folder inside your Portacle folder. You can see different subfolders named `bd`, `sn`, etc. that look just like the events we use to fill our structures. And in fact, when Mégra is started, it scans through the `megra-samples` folder and creates one event representation for each of the subfolders. Now, when the event is called "as is", Mégra will just choose a random sample from that subfolder.

```lisp
;; execute multiple times and see how it changes
(s 'ran ()
  (nuc 'samp (bd) :dur 400))
```

Now, is there a more controlled way ? Yes there is ! You can provide one or more keywords to pick a sample. If you run the following code multiple times, the sound shouldn't change:

```lisp
(s 'ran ()
  (nuc 'samp (bd 'bd808) :dur 400)) ;; look through the "bd"  folder, change the keyword, and see what happens!
```


## Synth Sounds
