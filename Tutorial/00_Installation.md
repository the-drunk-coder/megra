# MEGRA INSTALLATION GUIDE

## DISCLAIMER
This guide is still on "expert" mode, like with so many
projects in their early stage, and probably incomplete.

I'm constantly working on making things more frictionless,
but i'm not quite there yet.

In the meanwhile, contact me per email (nik@parkellipsen.de)
and i'd be glad to help you making things work. 

## OS SUPPORT
Megra has been developed on Linux and thus, is relatively well-tested on Linux.
Most distributions should have the required software available in their package
repositories (for version requirements, see below). 

The OSX (w/ portaudio) version is somewhat working, check out the `osx` branch from this repository.
The dependencies should mostly be available from Homebrew.
A known limitation is that it's currently not possible to use scsynth and MIDI in precise
sync, as the synchronization uses some properties of the Incudine library that are
dependent on JACK. If you've JACK running on OSX, just use the regular version, i guess ?
On OSX, you might just dismiss the warnings about jackmidi that occur during startup.

I haven't worked with Windows in a while, i know that it's possible to compile
JACK and SBCL, but i've got no idea how they perform.

## REQUIREMENTS

Now install:
- JACK (jack2 preferred)
- SuperCollider (> 3.8, higher recommended ... )
- SC3-Plugins
- git
- sbcl (1.3.19 or higher have been tested ... )
- emacs (25.2.x have been tested, currently the only interface)
- slime (easiest is to install from MELPA, `M-x package-install RET slime RET RET`)
- curl (to install quicklisp)
- gpg (to verify quicklisp)
- quicklisp (follow guide on quicklisp page, https://www.quicklisp.org)

On macOS, additionally install (using Homebrew):
- portaudio
- portmidi
- gsl

osx homebrew command:
```
brew install curl gpg portaudio portmidi gsl
```

Clone the following git repositories (ideally into your quicklisp/local-projects folder):
- incudine:       https://github.com/titola/incudine
- common music:   https://github.com/ormf/cm
- cm-incudine:    https://github.com/ormf/cm-incudine
- fudi-incudine:  https://github.com/ormf/fudi-incudine
- cl-libsndfile:  https://github.com/ghollisjr/cl-libsndfile
- cl-fad:         https://github.com/edicl/cl-fad

```
cd </path/to/quicklisp>/local-projects
git clone https://github.com/titola/incudine
git clone https://github.com/ormf/cm
git clone https://github.com/ormf/cm-incudine
git clone https://github.com/ormf/fudi-incudine
git clone https://github.com/ghollisjr/cl-libsndfile
git clone https://github.com/edicl/cl-fad
```

If you haven't done so, clone this repository to the desired location.
```
cd <desired/location>
git clone https://github.com/the-drunk-coder/megra
;; osx branch if needed
git checkout osx
```

## SETUP

Pick one of the examples, depending on your operating system, from the `incudinerc_examples`
and copy it to `~/.incudinerc`:

```
cp <megra-repo>/incudinerc_examples/incudinerc_[osx|linux] ~/.incudinerc
```

If you have `cl-collider` installed, add `(setq *osc-package-nicknames* nil)` 
to your `.incudinerc` to avoid package name conflicts (if you don't know what
this is about you can just ignore it ...).

Be sure to enable jackmidi if you are on linux. It's easier that way !
Otherwise you might encounter errors that the `jackmidi` package is not
available.

Add the following lines to your `~/.emacs`:

```
;; for a nicer SLIME REPL
(require 'slime)
(slime-setup '(slime-fancy))

;; set SBCL as the LISP interpreter for slime ... 
;; arch linux (setq inferior-lisp-program "/usr/bin/sbcl --control-stack-size 50000")
(setq inferior-lisp-program "/usr/local/bin/sbcl --control-stack-size 50000")
```

Update your quicklisp distributions (open a lisp file and
evaluate '(ql:update-all-dists)').

Then start emacs and slime (`M-x slime RET`) and evaluate (`C-M-x` on desired line):
- `(ql:quickload "quicklisp-slime-helper")` (practical)
- `(ql:quickload "closer-mop")`
- `(ql:quickload "cm-incudine")` (ACCEPT all errors on osx)

Now, in the file "megra-load.lisp" you'll find two paths:

- `*megra-root*` (points to megra repo)
- `*sample-root*` (points to samples used for "grain*" events)
- `*sample-type*` (if you want to use your tidal samples, set to wav !)

Set them accordingly ... 

Add the following lines to your .emacs:

```
(setq megra-root "<path-to-megra-repo>") ;; no trailing '/' !  
(push megra-root load-path)
(require 'incudine-megra)
```

Fire up SuperCollider and load the synthdefs found in `megra-supercollider-synths.scd`.

Now, start ScSynth (and JACK on linux), open the file `megra-playground.megra`, start
Megra (`C-c m` ... again, ACCEPT all errors on osx) and evaluate the first expression 
(move cursor to expression and hit `C-RET`).

You should be able to hear some sound now ... 

## Samples

Samples (for the grain event) are organized, in a tidal-like
manner, in folders. 
In the file "megra-load.lisp" you can configure the sample root,
the sample type (flac or wav) and the name of the impulse response
needed for the reverb (without file extension).
There aren't any samples in this repository except for the tutorial 
samples.
If you're already using tidal, you can simply set the `*sample-root*` 
to your Dirt samples folder, and set the type to "wav".



