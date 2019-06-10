# Mégra Tutorial - Appendix A: Events and Parameters

## Sample Event

**Syntax**: 
```lisp 
(<sample-type> <keywords> <parameters>)
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
| `:lp-freq-lfo-speed` | 0.0 | lowpass frequency lfo speed (experimental) |
| `:lp-freq-lfo-depth` | 0.0 | lowpass frequency lfo depth (experimental) |
| `:lp-freq-lfo-phase` | 0.0 | lowpass frequency lfo phase (experimental) |





