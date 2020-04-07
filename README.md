# Mégra

Mégra is a mini-language to make music with variable-order markov chains
and some other stochastic shenanigans.

**This is Mégra 1.x ! Mégra 2.x is in the making and will bring some breaking changes, so stay tuned !**

## Installation
There are currently two ways to install. One uses **Portacle** as the underlying editor 
and is comparatively easy. You can find the instructions here:

https://github.com/the-drunk-coder/megra/blob/master/Tutorial/00_Installation.md

If you're familar with Common Lisp and Emacs, and want to integrate Mégra into your current
Emacs installation, please find some hints here:

https://github.com/the-drunk-coder/megra/blob/master/Tutorial/old/00b_Installation_old.md

## Differences between 1.x and 2.x
Megra 2.x is a different beast than 1.x, event though most of the core concepts made it over.
Some features didn't make it over to 2.x because they were too complicated, or caused too much clutter.

Amongst those are:

* The branch/cut feature. There's more intuitive and concise methods like `(xdup)`that do something very similar.
* The chain group feature. I only used it once, it hasn't even been documented, and the `(sx)` environment does a better job.git 
* Controller input. This has been obsolete for a long time ...
* Manual cloning. Never documented, never used.
* The old tutorial. Megra has changed a lot, it didn't make sense any longer.
* The old chain macro. Use cmp instead.

In the very unlikely case that you were an early Mégra user and are missing one of the features, please tell me and I'll see if
I can bring them back, or think about a good alternative. Otherwise, you can still use the 1.x branch.

## Learning

Please find the work-in-progress tutorial it in:

https://github.com/the-drunk-coder/megra/blob/master/Tutorial/

## Technicalities

Mégra is an embedded domain-specific language based on **Common Lisp**,
Tito Latini's amazing **Incudine** library and **Common Music 2.x**, which
is old but working, and currently (inofficially and as needed) maintained
by Orm Finnendahl.

