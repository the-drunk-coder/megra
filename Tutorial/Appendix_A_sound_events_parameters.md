# Mégra Tutorial - Appendix A: Sound Events and Parameters

* [Sample Events](#sample-events) - play samples
* [Simple Synth Events](#simple-synth-events) - simple waves
* [Risset Event](#risset-event) - Risset bells
* [Pluck Event](#pluck-event) - Karplus-Strong
* [DX Rhodes Event](#dx-rhodes-event) - DX7-type Rhodes
* [Meow Event](#meow-event) - Meow !
* A [Note](#a-note-about-note-names) about Note Names

## Sample Events

**Syntax**: 
```lisp 
(<sample-type> <keywords> <keyword parameters>)
```

**Example** 
```lisp
(bd 'bd808 :lp-freq 1000 :rate 0.9)
```

### Parameters

| Parameter | Default | Description |
|-----------|:-------:|:-----------:|
| `:lvl`       | 0.3     | gain level |
| `:rate`      | 1.0     | sample playback rate |
| `:start`     | 0.0     | start within sample file, ratio |
| `:atk`       | 5       | gain envelope attack, in ms |
| `:rel`       | 5       | gain envelope release, in ms |
| `:dur`       | -       | overall event duration (including atk and rel), in ms |
| `:pos`       | 0.5     | stereo position (0.5 - center) |
| `:lp-freq`   | 19000   | lowpass filter frequency  |
| `:lp-q`      | 0.4     | lowpass filter q factor |
| `:lp-dist`   | 0.0     | lowpass filter distortion|
| `:hp-freq`   | 20      | highpass filter frequency  |
| `:hp-q`      | 0.4     | highpass filter q factor |
| `:pf-freq`   | 1000    | peak filter frequency  |
| `:pf-q`      | 10      | peak filter q factor |
| `:pf-gain`   | 0.0     | peak filter gain |
| `:rev`       | 0.0     | reverb amount |
| `:echo`      | 0.0     | echo amount |
| `:echorev`   | 0.0     | echo-reverb amount (experimental) |
| `:lp-freq-lfo-speed` | 0.0 | lowpass frequency lfo speed (experimental) |
| `:lp-freq-lfo-depth` | 0.0 | lowpass frequency lfo depth (experimental) |
| `:lp-freq-lfo-phase` | 0.0 | lowpass frequency lfo phase (experimental) |

## Simple Synth Events 

**Syntax**: 
```lisp 
(sine|saw|sqr|par|cub|tri|buzz <pitch> <keyword parameters>)
```

**Example** 
```lisp
(sine 110) ;; with frequency
(sine 'a2 :rev 0.1) ;; with note name and reverb
```

### Types
| Type |Description|
|-----------|:-------:|
| sine | simple sine wave |
| cub  | a sine like shape made of two cubic pieces (LFCub) |
| par  | a sine-like shape made of two parabolas and the integral of a triangular wave (LFPar) |
| tri  | a triangle wave |
| sqr  | a square wave   |
| saw  | a sawtooth wave |
| buzz  | band-limited impulse oscillator|

### Parameters

| Parameter | Default | Description |
|-----------|:-------:|:-----------:|
|  pitch     | 43     | pitch - might be frequency in hertz or quoted note name |
| `:lvl`       | 0.3     | gain level |
| `:atk`       | 5       | gain envelope attack, in ms |
| `:rel`       | 5       | gain envelope release, in ms |
| `:dur`       | -       | overall event duration (including atk and rel), in ms |
| `:pos`       | 0.5     | stereo position (0.5 - center) |
| `:lp-freq`   | 19000   | lowpass filter frequency  |
| `:lp-q`      | 0.4     | lowpass filter q factor |
| `:lp-dist`   | 0.0     | lowpass filter distortion|
| `:rev`       | 0.0     | reverb amount |
| `:echo`      | 0.0     | echo amount |
| `:echorev`   | 0.0     | echo-reverb amount (experimental) |
| `:pw`        | 0.5     | pulsewidth (ONLY `sqr`) |
| `:harm`      | 3       | number of harmonics (ONLY `buzz`), be careful with values over 10 ! |

## Risset Event

A simple risset bell event.

**Syntax**: 
```lisp 
(risset <pitch> <keyword parameters>)
```

**Example** 
```lisp
(risset 2000) ;; with frequency
(risset 'a5 :rev 0.1) ;; with note name and reverb
```
### Parameters

| Parameter | Default | Description |
|-----------|:-------:|:-----------:|
|  pitch     | 43     | pitch - might be frequency in hertz or quoted note name |
| `:lvl`       | 0.3     | gain level |
| `:atk`       | 5       | gain envelope attack, in ms |
| `:dec`       | 20       | gain envelope decay, in ms |
| `:sus`       | 50       | gain envelope sustain, in ms |
| `:rel`       | 5       | gain envelope release, in ms |
| `:pos`       | 0.5     | stereo position (0.5 - center) |
| `:lp-freq`   | 19000   | lowpass filter frequency  |
| `:lp-q`      | 0.4     | lowpass filter q factor |
| `:lp-dist`   | 0.0     | lowpass filter distortion|
| `:rev`       | 0.0     | reverb amount |
| `:echo`      | 0.0     | echo amount |
| `:echorev`   | 0.0     | echo-reverb amount (experimental) |

## Pluck Event

Karplus-Strong type pluck event.

**Syntax**: 
```lisp 
(pluck <pitch> <keyword parameters>)
```

**Example** 
```lisp
(pluck 2000) ;; with frequency
(pluck 'a5 :rev 0.1) ;; with note name and reverb
```

### Parameters

| Parameter | Default | Description |
|-----------|:-------:|:-----------:|
|  pitch     | 43     | pitch - might be frequency in hertz or quoted note name |
| `:lvl`       | 0.3     | gain level |
| `:pos`       | 0.5     | stereo position (0.5 - center) |
| `:dur`       | 512      | overall event duration, in ms |
| `:lp-freq`   | 19000   | lowpass filter frequency  |
| `:lp-q`      | 0.4     | lowpass filter q factor |
| `:lp-dist`   | 0.0     | lowpass filter distortion|
| `:rev`       | 0.0     | reverb amount |
| `:echo`      | 0.0     | echo amount |
| `:echorev`   | 0.0     | echo-reverb amount (experimental) |

## DX Rhodes Event

A DX7 Rhodes type of event.

**Syntax**: 
```lisp 
(dx-rhodes <pitch> <keyword parameters>)
```

**Example** 
```lisp
(dx-rhodes 900) ;; with frequency
(dx-rhodes 'a4 :rev 0.1) ;; with note name and reverb
```

### Parameters

| Parameter | Default | Description |
|-----------|:-------:|:-----------:|
|  pitch     | 43     | pitch - might be frequency in hertz or quoted note name |
| `:lvl`       | 0.3     | gain level |
| `:pos`       | 0.5     | stereo position (0.5 - center) |
| `:dur`       | 512      | overall event duration, in ms |
| `:lp-freq`   | 19000   | lowpass filter frequency  |
| `:lp-q`      | 0.4     | lowpass filter q factor |
| `:lp-dist`   | 0.0     | lowpass filter distortion|
| `:rev`       | 0.0     | reverb amount |
| `:echo`      | 0.0     | echo amount |
| `:echorev`   | 0.0     | echo-reverb amount (experimental) |
| `:lvl-lfo-speed` | 0.0 | level lfo speed |
| `:lvl-lfo-depth` | 0.0 | level lfo depth |
| `:lvl-lfo-phase` | 0.0 | level lfo phase |
| `:mix` | 0.2 | mix  |
| `:vel` | 0.8 | velocity (think of key velocity on a keyboard) |
| `:mod-idx` | 0.3 | FM modulation index |

## Meow Event

Meow.

**Syntax**: 
```lisp 
(meow <pitch> <keyword parameters>)
```

**Example** 
```lisp
(meow 2000) ;; with frequency
(meow 'a3 :rev 0.1) ;; with note name and reverb
```

### Parameters

| Parameter | Default | Description |
|-----------|:-------:|:-----------:|
|  pitch     | 43     | pitch - might be frequency in hertz or quoted note name |
| `:lvl`       | 0.3     | gain level |
| `:pos`       | 0.5     | stereo position (0.5 - center) |
| `:dur`       | 512      | overall event duration, in ms |
| `:lp-freq`   | 19000   | lowpass filter frequency  |
| `:lp-q`      | 0.4     | lowpass filter q factor |
| `:lp-dist`   | 0.0     | lowpass filter distortion|
| `:rev`       | 0.0     | reverb amount |
| `:echo`      | 0.0     | echo amount |
| `:echorev`   | 0.0     | echo-reverb amount (experimental) |

## A Note about Note Names
Note names follow the Common Music 2.x convention, where `'a4` is the concert pitch of 440hz, above the *middle c* which is `'c4`. `'cs4` denotes a sharp note, `'cf4` denotes a flat note. The sharp/flat schema is consistent over all notes.
