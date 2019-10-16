
//s = Server.local
//o = s.options
//o.numOutputBusChannels = 16
//o.numInputBusChannels = 0
//s.options = o
//s.boot

// reverb buffer hardcoded to 1 !!
SynthDef(\grain_8ch, {

	|
	bufnum=0, lvl=0.5, rate=1.0, start=0.0,

	//lowpass
	lp_freq=19000, lp_q=0.1, lp_dist=0.0, lp_freq_lfo_freq=0.0, lp_freq_lfo_depth = 0.0, lp_freq_lfo_phase = 0.0,

	// peak
	pf_freq=4000, pf_q=0.1, pf_gain=0.0,

	// hi-pass
	hp_freq=20, hp_q=0.3,

	// env
	a=0.01, length=1, r=0.02, pos=0.0
	|

	var lim_gain = min(lvl, 1.0);
	var cutenv = Env.new([0, lim_gain, lim_gain, 0], [a, length, r]);
	var outenv = Env.new([0, lim_gain, lim_gain, 0], [a, length + 1.5, r]);
	var sample = PlayBuf.ar(1, bufnum, rate, 1, startPos: BufDur.ir(bufnum) * 44100 * start, loop: 1) * EnvGen.ar(cutenv);
	var sample_hp = BHiPass.ar(sample, hp_freq, hp_q);
	var sample_peak = BPeakEQ.ar(sample_hp, pf_freq, pf_q, pf_gain);
	var sample_lp = LPF18.ar(sample_peak, lp_freq - ((lp_freq * lp_freq_lfo_depth) * SinOsc.ar(lp_freq_lfo_freq, lp_freq_lfo_phase)), lp_q, lp_dist);
	Out.ar(0, PanAz.ar(8, sample_lp * EnvGen.ar(outenv, doneAction:2), pos * 0.25, orientation: 0));
}).store;

// the typical filtered grain player, for 8 channels, with convolution reverb
SynthDef(\grain_8ch_rev, {
	|
	bufnum=0, lvl=0.5, rate=1.0, start=0.0,

	lp_freq=19000, lp_q=0.1, lp_dist=0.0, lp_freq_lfo_freq=0.8, lp_freq_lfo_depth = 1.0, lp_freq_lfo_phase = 0.5,

	pf_freq=4000, pf_q=0.1, pf_gain=0.0,

	hp_freq=20, hp_q=0.3, a=0.01,

	length=1, r=0.02, pos=0.0, rev=0.0
	|

	var lim_gain = min(lvl, 1.0);
	var cutenv = Env.new([0, lim_gain, lim_gain, 0], [a, length, r]);
	var sample = PlayBuf.ar(1, bufnum, rate, 1, startPos: BufDur.ir(bufnum) * 44100 * start, loop: 1) * EnvGen.ar(cutenv);
	var sample_hp = BHiPass.ar(sample, hp_freq, hp_q);
	var sample_peak = BPeakEQ.ar(sample_hp, pf_freq, pf_q, pf_gain);
	var sample_lp = LPF18.ar(sample_peak, lp_freq - ((lp_freq * lp_freq_lfo_depth) * SinOsc.ar(lp_freq_lfo_freq, lp_freq_lfo_phase)), lp_q, lp_dist);
	var sample_rev = PartConv.ar(sample_lp, 2048, 1, rev);
	var sample_out = (sample_lp * (1.0 - rev)) + sample_rev;
	var revenv = Env.new([0, lim_gain, lim_gain, 0], [a, length + 2.5, r]);
	Out.ar(0, PanAz.ar(8, sample_out *  EnvGen.ar(revenv, doneAction:2) , pos * 0.25, orientation: 0));
}).store;

SynthDef.new(\saw_8ch, { | freq=440, lvl=0.5, a=0.5, length=0.5, r=0.5, lp_freq=880, lp_q=1, lp_dist=0.5, pos=0.5 |
	var lim_cutoff = min(lp_freq,18000);
	var lim_freq = min(freq, 18000);
	var lim_gain = min(lvl, 1.1);
	var filterenv = Env.new([0, lp_freq, lp_freq, 0], [a, length, r]);
	var ampenv = Env.new([0, lim_gain, lim_gain, 0], [a, length, r]);
	//var filterenv = Env([freq, lim_cutoff, lim_cutoff * 0.5, lim_cutoff * 0.5, 0], [a, d, s, r]);
	//var ampenv = Env([0, lim_gain, lim_gain*0.7, lim_gain*0.7, 0], [a, d, s, r]);
	var snd = LPF18.ar(LFSaw.ar(freq), EnvGen.ar(filterenv), lp_q, lp_dist) * EnvGen.ar(ampenv, doneAction: 2);
	Out.ar(0, PanAz.ar(8, snd, pos * 0.25, orientation: 0));
}).store;

SynthDef.new(\saw_8ch_rev, { | freq=440, lvl=0.5, a=0.3, length, r, lp_freq, lp_q, lp_dist, pos, rev |
	var lim_cutoff = min(lp_freq,15000);
	var lim_freq = min(freq, 15000);
	var lim_gain = min(lvl, 1.1);
	var filterenv = Env.new([0, lim_cutoff, lim_cutoff, 0], [a, length, r]);
	var ampenv = Env.new([0, lim_gain, lim_gain, 0], [a, length, r]);
	var snd = LPF18.ar(LFSaw.ar(freq), EnvGen.ar(filterenv), lp_q, lp_dist) * EnvGen.ar(ampenv);
	var revsnd = PartConv.ar(snd, 2048, 1, rev);
	var mixsnd = (snd * (1.0 - rev)) + revsnd;
	var outenv = Env.new([0, lim_gain, lim_gain, 0], [a, length + 2, r]);
	Out.ar(0, PanAz.ar(8, mixsnd *  EnvGen.ar(outenv, doneAction:2), pos * 0.25, orientation: 0));
}).store;

SynthDef(\sine_8ch, {| freq=1000, lvl=0.5, lp_freq=19000,
	                   lp_q=0.1, lp_dist=0.0,
	                   a=0.01, length=1, r=0.02, pos=0.0 |
	var lim_gain = min(lvl, 1.0);
	var cutenv = Env.new([0, lim_gain, lim_gain, 0], [a, length, r]);
	var outenv = Env.new([0, lim_gain, lim_gain, 0], [a, length + 2.5, r]);
	var sample =  SinOsc.ar(freq) * EnvGen.ar(cutenv);
	var sample_lp = LPF18.ar(sample, lp_freq, lp_q, lp_dist);
	Out.ar(0, PanAz.ar(8, sample_lp * EnvGen.ar(outenv, doneAction:2), pos*0.25, orientation:0));
}).store;

SynthDef(\sine_8ch_rev, {|  freq=1000, lvl=0.5, lp_freq=19000, lp_q=0.1,
	                        lp_dist=0.0, a=0.01, length=1, r=0.02, pos=0.0, rev=0.0 |
	var lim_gain = min(lvl, 1.0);
	var cutenv = Env.new([0, lim_gain, lim_gain, 0], [a, length, r]);
	var outenv = Env.new([0, lim_gain, lim_gain, 0], [a, length + 2.5, r]);
	var sample = SinOsc.ar(freq) * EnvGen.ar(cutenv);
	var sample_lp = LPF18.ar(sample, lp_freq, lp_q, lp_dist);
	var sample_rev = PartConv.ar(sample_lp, 2048, 1, rev);
	var sample_out = (sample_lp * (1.0 - rev)) + sample_rev;
	Out.ar(0, PanAz.ar(8, sample_out *  EnvGen.ar(outenv, doneAction:2) , pos*0.25, orientation:0));
}).store;

SynthDef(\tri_8ch, {| freq=1000, lvl=0.5, lp_freq=19000,
	                   lp_q=0.1, lp_dist=0.0,
	                   a=0.01, length=1, r=0.02, pos=0.0 |
	var lim_gain = min(lvl, 1.0);
	var cutenv = Env.new([0, lim_gain, lim_gain, 0], [a, length, r]);
	var outenv = Env.new([0, lim_gain, lim_gain, 0], [a, length + 2.5, r]);
	var sample =  LFTri.ar(freq) * EnvGen.ar(cutenv);
	var sample_lp = LPF18.ar(sample, lp_freq, lp_q, lp_dist);
	Out.ar(0, PanAz.ar(8, sample_lp * EnvGen.ar(outenv, doneAction:2), pos*0.25, orientation:0));
}).store;

SynthDef(\tri_8ch_rev, {|  freq=1000, lvl=0.5, lp_freq=19000, lp_q=0.1,
	                        lp_dist=0.0, a=0.01, length=1, r=0.02, pos=0.0, rev=0.0 |
	var lim_gain = min(lvl, 1.0);
	var cutenv = Env.new([0, lim_gain, lim_gain, 0], [a, length, r]);
	var outenv = Env.new([0, lim_gain, lim_gain, 0], [a, length + 2.5, r]);
	var sample = LFTri.ar(freq) * EnvGen.ar(cutenv);
	var sample_lp = LPF18.ar(sample, lp_freq, lp_q, lp_dist);
	var sample_rev = PartConv.ar(sample_lp, 2048, 1, rev);
	var sample_out = (sample_lp * (1.0 - rev)) + sample_rev;
	Out.ar(0, PanAz.ar(8, sample_out *  EnvGen.ar(outenv, doneAction:2) , pos*0.25, orientation:0));
}).store;

SynthDef(\par_8ch, {| freq=1000, lvl=0.5, lp_freq=19000,
	                   lp_q=0.1, lp_dist=0.0,
	                   a=0.01, length=1, r=0.02, pos=0.0 |
	var lim_gain = min(lvl, 1.0);
	var cutenv = Env.new([0, lim_gain, lim_gain, 0], [a, length, r]);
	var outenv = Env.new([0, lim_gain, lim_gain, 0], [a, length + 2.5, r]);
	var sample =  LFPar.ar(freq) * EnvGen.ar(cutenv);
	var sample_lp = LPF18.ar(sample, lp_freq, lp_q, lp_dist);
	Out.ar(0, PanAz.ar(8, sample_lp * EnvGen.ar(outenv, doneAction:2), pos*0.25, orientation:0));
}).store;

SynthDef(\par_8ch_rev, {|  freq=1000, lvl=0.5, lp_freq=19000, lp_q=0.1,
	                        lp_dist=0.0, a=0.01, length=1, r=0.02, pos=0.0, rev=0.0 |
	var lim_gain = min(lvl, 1.0);
	var cutenv = Env.new([0, lim_gain, lim_gain, 0], [a, length, r]);
	var outenv = Env.new([0, lim_gain, lim_gain, 0], [a, length + 2.5, r]);
	var sample = LFPar.ar(freq) * EnvGen.ar(cutenv);
	var sample_lp = LPF18.ar(sample, lp_freq, lp_q, lp_dist);
	var sample_rev = PartConv.ar(sample_lp, 2048, 1, rev);
	var sample_out = (sample_lp * (1.0 - rev)) + sample_rev;
	Out.ar(0, PanAz.ar(8, sample_out *  EnvGen.ar(outenv, doneAction:2) , pos*0.25, orientation:0));
}).store;

SynthDef(\cub_8ch, {| freq=1000, lvl=0.5, lp_freq=19000,
	                   lp_q=0.1, lp_dist=0.0,
	                   a=0.01, length=1, r=0.02, pos=0.0 |
	var lim_gain = min(lvl, 1.0);
	var cutenv = Env.new([0, lim_gain, lim_gain, 0], [a, length, r]);
	var outenv = Env.new([0, lim_gain, lim_gain, 0], [a, length + 2.5, r]);
	var sample =  LFCub.ar(freq) * EnvGen.ar(cutenv);
	var sample_lp = LPF18.ar(sample, lp_freq, lp_q, lp_dist);
	Out.ar(0, PanAz.ar(8, sample_lp * EnvGen.ar(outenv, doneAction:2), pos*0.25, orientation:0));
}).store;

SynthDef(\cub_8ch_rev, {|  freq=1000, lvl=0.5, lp_freq=19000, lp_q=0.1,
	                        lp_dist=0.0, a=0.01, length=1, r=0.02, pos=0.0, rev=0.0 |
	var lim_gain = min(lvl, 1.0);
	var cutenv = Env.new([0, lim_gain, lim_gain, 0], [a, length, r]);
	var outenv = Env.new([0, lim_gain, lim_gain, 0], [a, length + 2.5, r]);
	var sample = LFCub.ar(freq) * EnvGen.ar(cutenv);
	var sample_lp = LPF18.ar(sample, lp_freq, lp_q, lp_dist);
	var sample_rev = PartConv.ar(sample_lp, 2048, 1, rev);
	var sample_out = (sample_lp * (1.0 - rev)) + sample_rev;
	Out.ar(0, PanAz.ar(8, sample_out *  EnvGen.ar(outenv, doneAction:2) , pos*0.25, orientation:0));
}).store;

SynthDef(\buzz_8ch, {| freq=1000, lvl=0.5, harm=3, lp_freq=19000,
	                   lp_q=0.1, lp_dist=0.0,
	                   a=0.01, length=1, r=0.02, pos=0.0 |
	var lim_gain = min(lvl, 1.0);
	var cutenv = Env.new([0, lim_gain, lim_gain, 0], [a, length, r]);
	var outenv = Env.new([0, lim_gain, lim_gain, 0], [a, length + 2.5, r]);
	var sample =  Blip.ar(freq, harm) * EnvGen.ar(cutenv);
	var sample_lp = LPF18.ar(sample, lp_freq, lp_q, lp_dist);
	Out.ar(0, PanAz.ar(8, sample_lp * EnvGen.ar(outenv, doneAction:2), pos*0.25, orientation:0));
}).store;

SynthDef(\buzz_2ch_rev, {|freq=1000, lvl=0.5, harm=3, lp_freq=19000, lp_q=0.1,
	                      lp_dist=0.0, a=0.01, length=1, r=0.02, pos=0.0, rev=0.0 |
	var lim_gain = min(lvl, 1.0);
	var cutenv = Env.new([0, lim_gain, lim_gain, 0], [a, length, r]);
	var outenv = Env.new([0, lim_gain, lim_gain, 0], [a, length + 2.5, r]);
	var sample = Blip.ar(freq, harm)  * EnvGen.ar(cutenv);
	var sample_lp = LPF18.ar(sample, lp_freq, lp_q, lp_dist);
	var sample_rev = PartConv.ar(sample_lp, 2048, 1, rev);
	var sample_out = (sample_lp * (1.0 - rev)) + sample_rev;
	Out.ar(0, PanAz.ar(8, sample_out *  EnvGen.ar(outenv, doneAction:2) , pos*0.25, orientation:0));
}).store;

SynthDef.new(\sqr_8ch, { | freq=440, lvl=0.5, a=0.5, length=0.5, r=0.5, lp_freq=880, lp_q=1, lp_dist=0.5, pos=0.5, pulsewidth=0.5 |
	var lim_cutoff = min(lp_freq,18000);
	var lim_freq = min(freq, 18000);
	var lim_gain = min(lvl, 1.1);
	var filterenv = Env.new([0, lp_freq, lp_freq, 0], [a, length, r]);
	var ampenv = Env.new([0, lim_gain, lim_gain, 0], [a, length, r]);
	//var filterenv = Env([freq, lim_cutoff, lim_cutoff * 0.5, lim_cutoff * 0.5, 0], [a, d, s, r]);
	//var ampenv = Env([0, lim_gain, lim_gain*0.7, lim_gain*0.7, 0], [a, d, s, r]);
	var snd = LPF18.ar(LFPulse.ar(freq, width:pulsewidth), EnvGen.ar(filterenv), lp_q, lp_dist) * EnvGen.ar(ampenv, doneAction: 2);
	Out.ar(0, PanAz.ar(8, snd, pos*0.25, orientation:0))
}).store;

SynthDef.new(\sqr_8ch_rev, { | freq=440, lvl=0.5, a=0.3, length, r, lp_freq, lp_q, lp_dist, pos, rev, pulsewidth |
	var lim_cutoff = min(lp_freq,15000);
	var lim_freq = min(freq, 15000);
	var lim_gain = min(lvl, 1.1);
	var filterenv = Env.new([0, lim_cutoff, lim_cutoff, 0], [a, length, r]);
	var ampenv = Env.new([0, lim_gain, lim_gain, 0], [a, length, r]);
	var snd = LPF18.ar(LFPulse.ar(freq, width:pulsewidth), EnvGen.ar(filterenv), lp_q, lp_dist) * EnvGen.ar(ampenv);
	var revsnd = PartConv.ar(snd, 2048, 1, rev);
	var mixsnd = (snd * (1.0 - rev)) + revsnd;
	var outenv = Env.new([0, lim_gain, lim_gain, 0], [a, length + 2, r]);
	Out.ar(0, PanAz.ar(8, mixsnd *  EnvGen.ar(outenv, doneAction:2), pos*0.25, orientation:0));
}).store;

// Based on the risset port by Frederik Oloffson
SynthDef(\risset_8ch, {|freq, lvl, a, d, s, r, pos, lp_freq, lp_q, lp_dist|
	var length = a + d + s + r;
	var amps= #[1, 0.67, 1, 1.8, 2.67, 1.67, 1.46, 1.33, 1.33, 1, 1.33];
	var durs= #[1, 0.9, 0.65, 0.55, 0.325, 0.35, 0.25, 0.2, 0.15, 0.1, 0.075];
	var frqs= #[0.56, 0.56, 0.92, 0.92, 1.19, 1.7, 2, 2.74, 3, 3.76, 4.07];
	var dets= #[0, 1, 0, 1.7, 0, 0, 0, 0, 0, 0, 0];
	var src= LPF18.ar(Mix.fill(11, {|i|
		var env= EnvGen.ar(Env.perc(0.005, length * durs[i], amps[i], -4.5), 1);
		SinOsc.ar(freq*frqs[i]+dets[i], 0, lvl * env);
	}), lp_freq, lp_q, lp_dist);
	var outenv = Env.new([0, lvl, lvl, 0], [a, d + s + r + 1.5, 0.01]);
	Out.ar(0, PanAz.ar(8, src * EnvGen.ar(outenv, doneAction:2), pos * 0.25, orientation: 0));
}).store;

// Based on the risset port by Frederik Oloffson
SynthDef(\risset_8ch_rev, {|freq, lvl, a, d, s, r, pos, rev, lp_freq, lp_q, lp_dist|
	var length = a + d + s + r;
	var amps= #[1, 0.67, 1, 1.8, 2.67, 1.67, 1.46, 1.33, 1.33, 1, 1.33];
	var durs= #[1, 0.9, 0.65, 0.55, 0.325, 0.35, 0.25, 0.2, 0.15, 0.1, 0.075];
	var frqs= #[0.56, 0.56, 0.92, 0.92, 1.19, 1.7, 2, 2.74, 3, 3.76, 4.07];
	var dets= #[0, 1, 0, 1.7, 0, 0, 0, 0, 0, 0, 0];
	var src= LPF18.ar(Mix.fill(11, {|i|
		var env= EnvGen.ar(Env.perc(0.005, length*durs[i], amps[i], -4.5), 1);
		SinOsc.ar(freq*frqs[i]+dets[i], 0, lvl * env);
	}), lp_freq, lp_q, lp_dist);
	var revsrc = PartConv.ar(src, 2048, 1, rev);
	var mixsrc = (src * (1.0 - rev)) + revsrc;
	var outenv = Env.new([0, lvl, lvl, 0], [a, d + s + r + 1.9, 0.1]);
	Out.ar(0, PanAz.ar(8, mixsrc * EnvGen.ar(outenv, doneAction:2), pos * 0.25, orientation: 0));
}).store;

// Karplus-Strong sound ...
SynthDef(\pluck_8ch, {|freq=440, pos=0, length=0.5, lvl=0.3, lp_freq=12000, lp_q=0.1, lp_dist=0.1 |
	var lim_cutoff = min(lp_freq, 20000);
	var lim_freq = min(freq, 15000);
	var pluck, period, string, snd, filterenv, outenv;
	pluck = PinkNoise.ar(Decay.kr(Impulse.kr(0.005), 0.05));
	period = freq.reciprocal;
	string = CombL.ar(pluck, period, period, length * 6);
	string = LeakDC.ar(LPF18.ar(Pan2.ar(string, pos), lp_freq, lp_q, lp_dist));
	filterenv = Env([lim_freq, lim_cutoff, lim_cutoff * 0.8, lim_cutoff * 0.8, 0], [3, 60, 100, 100]);
	snd = LPF18.ar(string, EnvGen.ar(filterenv), lp_q, lp_dist) * lvl;
	outenv = Env.new([0, lvl, lvl, 0], [0.002, length, 0.1]);
	Out.ar(0, PanAz.ar(8, string * EnvGen.ar(outenv, doneAction:2), pos * 0.25, orientation:0))
}).store;

SynthDef(\pluck_8ch_rev, {|freq=440, pos=0, length=0.5, lvl=0.3, lp_freq=12000, lp_q, lp_dist, rev=0.1|
	var lim_cutoff = min(lp_freq,20000);
	var lim_freq = min(freq, 15000);
	var pluck, period, string, snd, revsnd, mixsnd, filterenv, outenv;
	pluck = PinkNoise.ar(Decay.kr(Impulse.kr(0.005), 0.05));
	period = freq.reciprocal;
	string = CombL.ar(pluck, period, period, length*6);
	string = LeakDC.ar(LPF18.ar(Pan2.ar(string, pos), lp_freq, lp_q, lp_dist));
	filterenv = Env([lim_freq, lim_cutoff, lim_cutoff * 0.8, lim_cutoff * 0.8, 0], [3, 60, 100, 100]);
	snd = BLowPass4.ar(string, EnvGen.ar(filterenv)) * lvl;
	revsnd = PartConv.ar(snd, 2048, 1, rev);
	mixsnd = (snd * (1.0 - rev)) + revsnd;
	outenv = Env.new([0, lvl, lvl, 0], [0.007, length + 1.9, 0.01]);
	Out.ar(0, PanAz.ar(8, mixsnd * EnvGen.ar(outenv, doneAction:2), pos * 0.25, orientation:0))
}).store;

/* FM Rhodes Synthesizer (from sccode, with slight adaptions) */
SynthDef(\rhodey_sc_8ch, {
    |
    // standard meanings
	freq = 440, gate = 1, pos = 0, lvl = 0.1,
    // all of these range from 0 to 1
    vel = 0.8, modIndex = 0.2, mix = 0.2, lfoSpeed = 0.8, lfoDepth = 0.07
    |
    var env1, env2, env3, env4;
    var osc1, osc2, osc3, osc4, snd, outenv;

    lfoSpeed = lfoSpeed * 12;

    freq = freq * 2;

    env1 = EnvGen.ar(Env.adsr(0.001, 1.25, 0.0, 0.04, curve: \lin));
    env2 = EnvGen.ar(Env.adsr(0.001, 1.00, 0.0, 0.04, curve: \lin));
    env3 = EnvGen.ar(Env.adsr(0.001, 1.50, 0.0, 0.04, curve: \lin));
    env4 = EnvGen.ar(Env.adsr(0.001, 1.50, 0.0, 0.04, curve: \lin));

    osc4 = SinOsc.ar(freq * 0.5) * 2pi * 2 * 0.535887 * modIndex * env4 * vel;
    osc3 = SinOsc.ar(freq, osc4) * env3 * vel;
    osc2 = SinOsc.ar(freq * 15) * 2pi * 0.108819 * env2 * vel;
    osc1 = SinOsc.ar(freq, osc2) * env1 * vel;
    snd = Mix((osc3 * (1 - mix)) + (osc1 * mix));
    snd = snd * (SinOsc.ar(lfoSpeed) * lfoDepth + 1);

    // using the doneAction: 2 on the other envs can create clicks (bc of the linear curve maybe?)
	outenv = Env.new([0, lvl, lvl, 0], [0, 2.0, 0.1]);
    snd = snd * EnvGen.ar(outenv, doneAction: 2);

    Out.ar(0, PanAz.ar(8, snd, pos*0.25, lvl, orientation:0));
}).store;

/* FM Rhodes Synthesizer (from sccode, with slight adaptions) */
SynthDef(\rhodey_sc_8ch_rev, {
    |
    // standard meanings
	freq = 440, gate = 1, pos = 0, lvl = 0.1,
    // all of these range from 0 to 1
    vel = 0.8, modIndex = 0.2, mix = 0.2, lfoSpeed = 0.8, lfoDepth = 0.07, rev=0.0
    |
    var env1, env2, env3, env4;
    var osc1, osc2, osc3, osc4, snd, revsnd, mixsnd, outenv;

    lfoSpeed = lfoSpeed * 12;

    freq = freq * 2;

    env1 = EnvGen.ar(Env.adsr(0.001, 1.25, 0.0, 0.04, curve: \lin));
    env2 = EnvGen.ar(Env.adsr(0.001, 1.00, 0.0, 0.04, curve: \lin));
    env3 = EnvGen.ar(Env.adsr(0.001, 1.50, 0.0, 0.04, curve: \lin));
    env4 = EnvGen.ar(Env.adsr(0.001, 1.50, 0.0, 0.04, curve: \lin));

    osc4 = SinOsc.ar(freq * 0.5) * 2pi * 2 * 0.535887 * modIndex * env4 * vel;
    osc3 = SinOsc.ar(freq, osc4) * env3 * vel;
    osc2 = SinOsc.ar(freq * 15) * 2pi * 0.108819 * env2 * vel;
    osc1 = SinOsc.ar(freq, osc2) * env1 * vel;
    snd = Mix((osc3 * (1 - mix)) + (osc1 * mix));
    snd = snd * (SinOsc.ar(lfoSpeed) * lfoDepth + 1);

    // using the doneAction: 2 on the other envs can create clicks (bc of the linear curve maybe?)
    snd = snd * EnvGen.ar(Env.asr(0, 1, 0.1), gate);
	revsnd = PartConv.ar(snd, 2048, 1, rev);
	mixsnd = (snd * (1.0 - rev)) + revsnd;

	outenv = Env.new([0, lvl, lvl, 0], [0, 2.0, 0.1]);

    Out.ar(0,  PanAz.ar(8, mixsnd * EnvGen.ar(outenv, doneAction:2), pos, lvl));
}).store;

0.exit;
