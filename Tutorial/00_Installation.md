# MEGRA INSTALLATION GUIDE

## DISCLAIMER
This guide is still on "expert" mode, like with so many
projects in their early stage, and probably incomplete.

I'm constantly working on making things more frictionless,
but i'm not there yet.

In the meanwhile, contact me, either per email (nik@parkellipsen.de)
	    or via the Livecoding slack channel (@parkellipsen),
	    and i'd be glad to help you making things work. 

## OS SUPPORT
This installation guide is based on Linux, specifically Arch Linux.
Most programs you need will be available with other distributions, but
usually an older version (at least in the official repositories).

Also, i found arch quite well-suited for making music. Anyway, if you prefer
another distribution, you'll probably find anything you need in your favourite
package manager (including Homebrew if you're working with OSX).

The OSX w/ portaudio version is somewhat working, check out the 'osx' branch from this repository.
a know limitation is that it's currently not possible to use scsynth and MIDI in precise
sync, as the synchronization uses some properties of the Incudine library that are
dependent on JACK. If you've JACK running on OSX, just use the regular version, i guess ?
On OSX, you might just dismiss the warnings about jackmidi that occur during startup.

I haven't worked with Windows in a while, i know that it's possible to compile
JACK and SBCL, but i've got no idea how they perform.

## REQUIREMENTS

Now install:
- JACK
- SuperCollider (> 3.7, higher recommended ... )
- SC3-Plugins
- git
- sbcl (1.3.19 or higher have been tested ... )
- emacs (25.2.x have been tested, currently the only interface)
- slime (easiest is to install from MELPA)
- quicklisp (follow guide on quicklisp page)

On macOS, additionally install (using homebrew):
- portaudio
- portmidi
- gsl

On macOS (using homebrew):
- portaudio
- portmidi
- gsl


Clone the following repositories (ideally in your quicklisp/local-projects folder):
- incudine:       https://github.com/titola/incudine
- common music:   https://github.com/ormf/cm
- cm-incudine:    https://github.com/ormf/cm-incudine
- fudi-incudine:  https://github.com/ormf/fudi-incudine
- cl-libsndfile:  https://github.com/ghollisjr/cl-libsndfile
- cl-fad:         https://github.com/edicl/cl-fad

## SETUP

Before you start, go to the incudine folder and copy the file
`incudinerc-example` to `~/.incudinerc`.

Be sure to enable jackmidi if you are on linux. It's easier that way !
Otherwise you might encounter errors that the `jackmidi` package is not
available.

Update your quicklisp distributions (open a lisp file and
evaluate '(ql:update-all-dists)')

Then evaluate:
- `(ql:quickload "quicklisp-slime-helper")` (practical)
- `(ql:quickload "cm-incudine")`
- `(ql:quickload "closer-mop")`

Now, in the file "megra-load.lisp" you'll find two paths:

- `*megra-root*` (points to megra repo)
- `*sample-root*` (points to samples used for "grain*" events)

Set them accordingly ... 

Add the following lines to your .emacs:

```
(setq megra-root "<path-to-megra-repo>") ;; no trailing '/' !  
(push megra-root load-path)
(require 'incudine-megra)
```

## Samples

Samples (for the grain event) are organized in a tidal-like
manner, in folders. 
In the file "megra-load.lisp" you can configure the sample root,
the sample type (flac or wav) and the name of the impulse response
needed for the reverb (without file extension).
There aren't any samples in this repository except for the tutorial 
samples.
If you're already using tidal, you can simply set the `*sample-root*` 
to your Dirt samples folder, and set the type to "wav".



