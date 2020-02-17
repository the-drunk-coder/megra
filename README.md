# Mégra

Mégra is a mini-language to make music with variable-order markov chains
and some other stochastic shenanigans.

## Installation
There are currently two ways to install. One uses **Portacle** as the underlying editor 
and is comparatively easy. You can find the instructions here:

https://github.com/the-drunk-coder/megra/blob/master/Tutorial/00_Installation.md

If you're familar with Common Lisp and Emacs, and want to integrate Mégra into your current
Emacs installation, please find some hints here:

https://github.com/the-drunk-coder/megra/blob/master/Tutorial/old/00b_Installation_old.md

## Differences between 1.x and 2.x
Some features didn't make it over to 2.x because they were too complicated, or caused too much clutter.

Amongst those are:

* The branch/cut feature. There's more intuitive and concise methods like `(xdup)`that do something very similar.
* The chain group feature. I only used it once, it hasn't even been documented, and the `(sx)` environment does a better job.git 
* Controller input. This has been obsolete for a long time ...

In the very unlikely case that you were an early Mégra user and are missing one of the features, please tell me and I'll see if
I can bring them back, or think about a good alternative. Otherwise, you can still use the 1.x branch.

## Learning

Currently, there are two Tutorials. 

The *new*, main tutorial is in the making. It provides more in-depth explanations
about the inner workings of Mégra. Please find it in:

https://github.com/the-drunk-coder/megra/blob/master/Tutorial/

The historical, *old* tutorial was written as the language
evolved. It covers a good lot of the language, but the order in which things are explained might not be 
all that reasonable. You can find it in:

https://github.com/the-drunk-coder/megra/blob/master/Tutorial/old

## Technicalities

Mégra is an embedded domain-specific language based on **Common Lisp**,
Tito Latini's amazing **Incudine** library and **Common Music 2.x**, which
is old but working, and currently (inofficially and as needed) maintained
by Orm Finnendahl.

