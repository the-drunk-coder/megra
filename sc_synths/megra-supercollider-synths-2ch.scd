// reverb
SynthDef(\reverb_2ch, {
	var in1, in2;
	var out1, out2;
	# in1, in2 = In.ar(2, 2);

	out1 = PartConv.ar(in1, 2048, 1, 1.0);
	out2 = PartConv.ar(in2, 2048, 1, 1.0);

	Out.ar(0, [out1, out2]);
}).store;

// echo
SynthDef(\echo_2ch, { |length = 0.2, fb = 0.43, sep = 0.012|
	var input = In.ar(4, 2);
	var output = Fb({

		arg feedback; // this will contain the delayed output from the Fb unit

		var left,right;
		var magic = LeakDC.ar(feedback*fb + input);
		magic = HPF.ar(magic, 100); // filter's on the feedback path
		magic = LPF.ar(magic, 5000);
		magic = magic.tanh; // and some more non-linearity in the form of distortion
		#left, right = magic; // let's have named variables for the left and right channels
		magic = [DelayC.ar(left, 1, LFNoise2.ar(4).range(0,sep)), DelayC.ar(right, 1, LFNoise2.ar(4).range(sep,0))]; // In addition to the main delay handled by the feedback quark, this adds separately modulated delays to the left and right channels, which with a small "sep" value creates a bit of spatialization

	},length);
	Out.ar(0, output);
}).store;

// echo and reverb
SynthDef(\echorev_2ch, { |length = 0.2, fb = 0.43, sep = 0.012|
	var in1, in2;
	var out1, out2;

	var input = In.ar(6, 2);
	var output = input + Fb({

		arg feedback; // this will contain the delayed output from the Fb unit

		var left,right;
		var magic = LeakDC.ar(feedback*fb + input);
		magic = HPF.ar(magic, 100); // filter's on the feedback path
		magic = LPF.ar(magic, 5000);
		magic = magic.tanh; // and some more non-linearity in the form of distortion
		#left, right = magic; // let's have named variables for the left and right channels
		magic = [DelayC.ar(left, 1, LFNoise2.ar(4).range(0,sep)), DelayC.ar(right, 1, LFNoise2.ar(4).range(sep,0))]; // In addition to the main delay handled by the feedback quark, this adds separately modulated delays to the left and right channels, which with a small "sep" value creates a bit of spatialization

	},length);

	# in1, in2 = output;

	out1 = PartConv.ar(in1, 2048, 1, 1.0);
	out2 = PartConv.ar(in2, 2048, 1, 1.0);

	Out.ar(0, [out1, out2]);
}).store;

// the typical filtered grain player, for two channels ...
SynthDef(\grain_2ch, {

	|
	bufnum=0, lvl=0.5, rate=1.0, start=0.0,

	//lowpass
	lp_freq=19000, lp_q=0.1, lp_dist=0.0, lp_freq_lfo_freq=0.0, lp_freq_lfo_depth = 0.0, lp_freq_lfo_phase = 0.0,

	// peak
	pf_freq=4000, pf_q=0.1, pf_gain=0.0,

	// hi-pass
	hp_freq=20, hp_q=0.3,

	// env
	a=0.01, length=1, r=0.02, pos=0.0, rev=0.0, echo=0.0, echorev=0.0
	|

	var lim_gain = min(lvl, 1.0);
	var cutenv = Env.new([0, lim_gain, lim_gain, 0], [a, length, r]);
	var outenv = Env.new([0, lim_gain, lim_gain, 0], [a, length + 1.5, r]);
	var sample = PlayBuf.ar(1, bufnum, rate, 1, startPos: BufDur.ir(bufnum) * 44100 * start, loop: 1) * EnvGen.ar(cutenv);
	var sample_hp = BHiPass.ar(sample, hp_freq, hp_q);
	var sample_peak = BPeakEQ.ar(sample_hp, pf_freq, pf_q, pf_gain);
	var sample_lp = LPF18.ar(sample_peak, lp_freq - ((lp_freq * lp_freq_lfo_depth) * SinOsc.ar(lp_freq_lfo_freq, lp_freq_lfo_phase)), lp_q, lp_dist);
	var out =  Pan2.ar(sample_lp * EnvGen.ar(outenv, doneAction:2), pos);
	Out.ar(0, out * (1.0 - rev));
	Out.ar(2, out * rev);
	Out.ar(4, out * echo);
	Out.ar(6, out * echorev);
}).store;

// filtered noise
SynthDef(\noise_2ch, {

	|
	lvl=0.5,

	//lowpass
	lp_freq=19000, lp_q=0.1, lp_dist=0.0, lp_freq_lfo_freq=0.0, lp_freq_lfo_depth = 0.0, lp_freq_lfo_phase = 0.0,

	// peak
	pf_freq=4000, pf_q=0.1, pf_gain=0.0,

	// hi-pass
	hp_freq=20, hp_q=0.3,

	// env
	a=0.01, length=1, r=0.02, pos=0.0, rev=0.0, echo=0.0, echorev=0.0
	|

	var lim_gain = min(lvl, 1.0);
	var cutenv = Env.new([0, lim_gain, lim_gain, 0], [a, length, r]);
	var outenv = Env.new([0, lim_gain, lim_gain, 0], [a, length + 1.5, r]);
	var sample = WhiteNoise.ar() * EnvGen.ar(cutenv);
	var sample_hp = BHiPass.ar(sample, hp_freq, hp_q);
	var sample_peak = BPeakEQ.ar(sample_hp, pf_freq, pf_q, pf_gain);
	var sample_lp = LPF18.ar(sample_peak, lp_freq - ((lp_freq * lp_freq_lfo_depth) * SinOsc.ar(lp_freq_lfo_freq, lp_freq_lfo_phase)), lp_q, lp_dist);
	var out =  Pan2.ar(sample_lp * EnvGen.ar(outenv, doneAction:2), pos);
	Out.ar(0, out * (1.0 - rev));
	Out.ar(2, out * rev);
	Out.ar(4, out * echo);
	Out.ar(6, out * echorev);
}).store;

// the typical filtered grain player, for two channels, wit 24db filters
SynthDef(\grain_2ch_24db, {

	|
	bufnum=0, lvl=0.5, rate=1.0, start=0.0,

	//lowpass
	lp_freq=19000, lp_q=0.1, lp_freq_lfo_freq=0.0, lp_freq_lfo_depth = 0.0, lp_freq_lfo_phase = 0.0,

	// peak
	pf_freq=4000, pf_q=0.1, pf_gain=0.0,

	// hi-pass
	hp_freq=20, hp_q=0.3,

	// env
	a=0.01, length=1, r=0.02, pos=0.0, rev=0.0, echo=0.0, echorev=0.0
	|

	var lim_gain = min(lvl, 1.0);
	var cutenv = Env.new([0, lim_gain, lim_gain, 0], [a, length, r]);
	var outenv = Env.new([0, lim_gain, lim_gain, 0], [a, length + 1.5, r]);
	var sample = PlayBuf.ar(1, bufnum, rate, 1, startPos: BufDur.ir(bufnum) * 44100 * start, loop: 1) * EnvGen.ar(cutenv);
	var sample_hp = BHiPass.ar(sample, hp_freq, hp_q);
	var sample_peak = BPeakEQ.ar(sample_hp, pf_freq, pf_q, pf_gain);
	var sample_lp = BLowPass4.ar(sample_peak, lp_freq - ((lp_freq * lp_freq_lfo_depth) * SinOsc.ar(lp_freq_lfo_freq, lp_freq_lfo_phase)), lp_q);
	var out =  Pan2.ar(sample_lp * EnvGen.ar(outenv, doneAction:2), pos);
	Out.ar(0, out * (1.0 - rev));
	Out.ar(2, out * rev);
	Out.ar(4, out * echo);
	Out.ar(6, out * echorev);
}).store;


// the typical filtered grain player, for two channels ...
SynthDef(\grain_2ch_nores, {

	|
	bufnum=0, lvl=0.5, rate=1.0, start=0.0,

	//lowpass
	lp_freq=19000, lp_freq_lfo_freq=0.0, lp_freq_lfo_depth = 0.0, lp_freq_lfo_phase = 0.0,

	// peak
	pf_freq=4000, pf_q=0.1, pf_gain=0.0,

	// hi-pass
	hp_freq=20,

	// env
	a=0.01, length=1, r=0.02, pos=0.0, rev=0.0, echo=0.0, echorev=0.0
	|

	var lim_gain = min(lvl, 1.0);
	var cutenv = Env.new([0, lim_gain, lim_gain, 0], [a, length, r]);
	var outenv = Env.new([0, lim_gain, lim_gain, 0], [a, length + 1.5, r]);
	var sample = PlayBuf.ar(1, bufnum, rate, 1, startPos: BufDur.ir(bufnum) * 44100 * start, loop: 1) * EnvGen.ar(cutenv);
	var sample_hp = HPF.ar(sample, hp_freq);
	var sample_peak = BPeakEQ.ar(sample_hp, pf_freq, pf_q, pf_gain);
	var sample_lp = LPF.ar(sample_peak, lp_freq - ((lp_freq * lp_freq_lfo_depth) * SinOsc.ar(lp_freq_lfo_freq, lp_freq_lfo_phase)));
	var out =  Pan2.ar(sample_lp * EnvGen.ar(outenv, doneAction:2), pos);
	Out.ar(0, out * (1.0 - rev));
	Out.ar(2, out * rev);
	Out.ar(4, out * echo);
	Out.ar(6, out * echorev);
}).store;

SynthDef(\sine_2ch, {| freq=1000, lvl=0.5, lp_freq=19000,
	                   lp_q=0.1, lp_dist=0.0,
	                   a=0.01, length=1, r=0.02, pos=0.0, rev=0.0, echo=0.0, echorev=0.0 |
	var lim_gain = min(lvl, 1.0);
	var cutenv = Env.new([0, lim_gain, lim_gain, 0], [a, length, r]);
	var outenv = Env.new([0, lim_gain, lim_gain, 0], [a, length + 2.5, r]);
	var sample =  SinOsc.ar(freq) * EnvGen.ar(cutenv);
	var sample_lp = LPF18.ar(sample, lp_freq, lp_q, lp_dist);
	var out =  Pan2.ar(sample_lp * EnvGen.ar(outenv, doneAction:2), pos);
	Out.ar(0, out * (1.0 - rev));
	Out.ar(2, out * rev);
	Out.ar(4, out * echo);
	Out.ar(6, out * echorev);
}).store;

SynthDef(\tri_2ch, {| freq=1000, lvl=0.5, lp_freq=19000,
	                   lp_q=0.1, lp_dist=0.0,
	                   a=0.01, length=1, r=0.02, pos=0.0, rev=0.0, echo=0.0, echorev=0.0 |
	var lim_gain = min(lvl, 1.0);
	var cutenv = Env.new([0, lim_gain, lim_gain, 0], [a, length, r]);
	var outenv = Env.new([0, lim_gain, lim_gain, 0], [a, length + 2.5, r]);
	var sample =  LFTri.ar(freq) * EnvGen.ar(cutenv);
	var sample_lp = LPF18.ar(sample, lp_freq, lp_q, lp_dist);
	var out =  Pan2.ar(sample_lp * EnvGen.ar(outenv, doneAction:2), pos);
	Out.ar(0, out * (1.0 - rev));
	Out.ar(2, out * rev);
	Out.ar(4, out * echo);
	Out.ar(6, out * echorev);
}).store;


SynthDef(\par_2ch, {| freq=1000, lvl=0.5, lp_freq=19000,
	                   lp_q=0.1, lp_dist=0.0,
	                   a=0.01, length=1, r=0.02, pos=0.0, rev=0.0, echo=0.0, echorev=0.0 |
	var lim_gain = min(lvl, 1.0);
	var cutenv = Env.new([0, lim_gain, lim_gain, 0], [a, length, r]);
	var outenv = Env.new([0, lim_gain, lim_gain, 0], [a, length + 2.5, r]);
	var sample =  LFPar.ar(freq) * EnvGen.ar(cutenv);
	var sample_lp = LPF18.ar(sample, lp_freq, lp_q, lp_dist);
	var out =  Pan2.ar(sample_lp * EnvGen.ar(outenv, doneAction:2), pos);
	Out.ar(0, out * (1.0 - rev));
	Out.ar(2, out * rev);
	Out.ar(4, out * echo);
	Out.ar(6, out * echorev);
}).store;

SynthDef(\cub_2ch, {| freq=1000, lvl=0.5, lp_freq=19000,
	                   lp_q=0.1, lp_dist=0.0,
	                   a=0.01, length=1, r=0.02, pos=0.0, rev=0.0, echo=0.0, echorev=0.0 |
	var lim_gain = min(lvl, 1.0);
	var cutenv = Env.new([0, lim_gain, lim_gain, 0], [a, length, r]);
	var outenv = Env.new([0, lim_gain, lim_gain, 0], [a, length + 2.5, r]);
	var sample =  LFCub.ar(freq) * EnvGen.ar(cutenv);
	var sample_lp = LPF18.ar(sample, lp_freq, lp_q, lp_dist);
	var out =  Pan2.ar(sample_lp * EnvGen.ar(outenv, doneAction:2), pos);
	Out.ar(0, out * (1.0 - rev));
	Out.ar(2, out * rev);
	Out.ar(4, out * echo);
	Out.ar(6, out * echorev);
}).store;

SynthDef(\buzz_2ch, {| freq=1000, lvl=0.5, harm=3, lp_freq=19000,
	                   lp_q=0.1, lp_dist=0.0,
	                   a=0.01, length=1, r=0.02, pos=0.0, rev=0.0, echo=0.0, echorev=0.0 |
	var lim_gain = min(lvl, 1.0);
	var cutenv = Env.new([0, lim_gain, lim_gain, 0], [a, length, r]);
	var outenv = Env.new([0, lim_gain, lim_gain, 0], [a, length + 2.5, r]);
	var sample = Blip.ar(freq, harm) * EnvGen.ar(cutenv);
	var sample_lp = LPF18.ar(sample, lp_freq, lp_q, lp_dist);
	var out =  Pan2.ar(sample_lp * EnvGen.ar(outenv, doneAction:2), pos);
	Out.ar(0, out * (1.0 - rev));
	Out.ar(2, out * rev);
	Out.ar(4, out * echo);
	Out.ar(6, out * echorev);
}).store;

SynthDef.new(\saw_2ch, { | freq=440, lvl=0.5, a=0.5, length=0.5, r=0.5, lp_freq=880, lp_q=1, lp_dist=0.5, pos=0.5, rev=0.0, echo=0.0, echorev=0.0 |
	var lim_cutoff = min(lp_freq,18000);
	var lim_freq = min(freq, 18000);
	var lim_gain = min(lvl, 1.1);
	var filterenv = Env.new([0, lp_freq, lp_freq, 0], [a, length, r]);
	var ampenv = Env.new([0, lim_gain, lim_gain, 0], [a, length, r]);
	//var filterenv = Env([freq, lim_cutoff, lim_cutoff * 0.5, lim_cutoff * 0.5, 0], [a, d, s, r]);
	//var ampenv = Env([0, lim_gain, lim_gain*0.7, lim_gain*0.7, 0], [a, d, s, r]);
	var snd = LPF18.ar(LFSaw.ar(freq), EnvGen.ar(filterenv), lp_q, lp_dist) * EnvGen.ar(ampenv, doneAction: 2);
	var out = Pan2.ar(snd, pos);
	Out.ar(0, out * (1.0 - rev));
	Out.ar(2, out * rev);
	Out.ar(4, out * echo);
	Out.ar(6, out * echorev);
}).store;

SynthDef.new(\sqr_2ch, { | freq=440, lvl=0.5, a=0.5, length=0.5, r=0.5, lp_freq=880, lp_q=1, lp_dist=0.5, pos=0.5, pulsewidth=0.5, rev=0.0, echo=0.0, echorev=0.0 |
	var lim_cutoff = min(lp_freq,18000);
	var lim_freq = min(freq, 18000);
	var lim_gain = min(lvl, 1.1);
	var filterenv = Env.new([0, lp_freq, lp_freq, 0], [a, length, r]);
	var ampenv = Env.new([0, lim_gain, lim_gain, 0], [a, length, r]);
	//var filterenv = Env([freq, lim_cutoff, lim_cutoff * 0.5, lim_cutoff * 0.5, 0], [a, d, s, r]);
	//var ampenv = Env([0, lim_gain, lim_gain*0.7, lim_gain*0.7, 0], [a, d, s, r]);
	var snd = LPF18.ar(LFPulse.ar(freq, width:pulsewidth), EnvGen.ar(filterenv), lp_q, lp_dist) * EnvGen.ar(ampenv, doneAction: 2);
	var out = Pan2.ar(snd, pos);
	Out.ar(0, out * (1.0 - rev));
	Out.ar(2, out * rev);
	Out.ar(4, out * echo);
	Out.ar(6, out * echorev);
}).store;

SynthDef.new(\saw_adsr_2ch, { | freq, lvl, a, d, s, r, lp_freq, lp_q, lp_dist, pos, rev=0.0, echo=0.0, echorev=0.0 |
	var lim_cutoff = min(lp_freq, 18000);
	var lim_freq = min(freq, 18000);
	var lim_gain = min(lvl, 1.1);
	var filterenv = Env([freq, lim_cutoff, lim_cutoff * 0.5, lim_cutoff * 0.5, 0], [a, d, s, r]);
	var ampenv = Env([0, lim_gain, lim_gain*0.7, lim_gain*0.7, 0], [a, d, s, r]);
	var snd = LPF18.ar(LFSaw.ar(freq), EnvGen.ar(filterenv), lp_q, lp_dist) * EnvGen.ar(ampenv, doneAction: 2);
	var out = Pan2.ar(snd, pos);
	Out.ar(0, out * (1.0 - rev));
	Out.ar(2, out * rev);
	Out.ar(4, out * echo);
	Out.ar(6, out * echorev);
}).store;

SynthDef.new(\sqr_adsr_2ch, { | freq, lvl, pulsewidth, a, d, s, r, lp_freq, lp_q, lp_dist, pos, rev=0.0, echo=0.0, echorev=0.0 |
	var lim_cutoff = min(lp_freq,15000);
	var lim_freq = min(freq, 15000);
	var lim_gain = min(lvl, 1.1);
       var filterenv = Env([freq, lim_cutoff, lim_cutoff * 0.5, lim_cutoff * 0.5, 0], [a, d, s, r]);
	var ampenv = Env([0, lim_gain, lim_gain*0.7, lim_gain*0.7, 0], [a, d, s, r]);
	var snd = LPF18.ar(LFPulse.ar(freq, width:pulsewidth), EnvGen.ar(filterenv), lp_q, lp_dist) * EnvGen.ar(ampenv, doneAction: 2);
	var out = Pan2.ar(snd, pos);
	Out.ar(0, out * (1.0 - rev));
	Out.ar(2, out * rev);
	Out.ar(4, out * echo);
	Out.ar(6, out * echorev);
}).store;

// Based on the risset port by Frederik Oloffson
SynthDef(\risset_2ch, {|freq, lvl, a, d, s, r, pos, lp_freq, lp_q, lp_dist, rev=0.0, echo=0.0, echorev=0.0|
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
	var out = Pan2.ar(src * EnvGen.ar(outenv, doneAction:2), pos);
	Out.ar(0, out * (1.0 - rev));
	Out.ar(2, out * rev);
	Out.ar(4, out * echo);
	Out.ar(6, out * echorev);
}).store;

// Karplus-Strong sound ...
SynthDef(\pluck_2ch, {|freq=440, pos=0, length=0.5, lvl=0.3, lp_freq=12000, lp_q=0.1, lp_dist=0.1, rev=0.0, echo=0.0, echorev=0.0 |
	var lim_cutoff = min(lp_freq, 20000);
	var lim_freq = min(freq, 15000);
	var pluck, period, string, snd, filterenv, outenv, out;
	pluck = PinkNoise.ar(Decay.kr(Impulse.kr(0.005), 0.05));
	period = freq.reciprocal;
	string = CombL.ar(pluck, period, period, length * 6);
	string = LeakDC.ar(LPF18.ar(Pan2.ar(string, pos), lp_freq, lp_q, lp_dist));
	filterenv = Env([lim_freq, lim_cutoff, lim_cutoff * 0.8, lim_cutoff * 0.8, 0], [3, 60, 100, 100]);
	snd = LPF18.ar(string, EnvGen.ar(filterenv), lp_q, lp_dist) * lvl;
	outenv = Env.new([0, lvl, lvl, 0], [0.002, length, 0.1]);
	out = Pan2.ar(snd * EnvGen.ar(outenv, doneAction:2), pos);
	Out.ar(0, out * (1.0 - rev));
	Out.ar(2, out * rev);
	Out.ar(4, out * echo);
	Out.ar(6, out * echorev);
}).store;

SynthDef.new(\meow_2ch, { | freq, lvl, a, d, s, r, lp_freq, lp_q, lp_dist, pos, rev=0.0, echo=0.0, echorev=0.0 |
	var lim_cutoff = min(lp_freq,15000);
	var lim_freq = min(freq, 15000);
	var lim_gain = min(lvl, 1.1);
	var filterenv = Env.new([lim_freq, lim_cutoff, lim_cutoff * 0.5, lim_cutoff * 0.5, 0], [a, d, s, r]);
	var ampenv = Env.new([0, lim_gain, lim_gain*0.7, lim_gain*0.7, 0], [a, d, s, r]);
	var snd = LPF18.ar(StkInst.ar(Stk.at("Bowed"),freq, 1, lvl, 0.5,[9, 62, 4,17,12,52,1,4,23,128,128]), EnvGen.ar(filterenv), lp_q, lp_dist);
	var out = Pan2.ar(snd * EnvGen.ar(ampenv, doneAction:2), pos);
	Out.ar(0, out * (1.0 - rev));
	Out.ar(2, out * rev);
	Out.ar(4, out * echo);
	Out.ar(6, out * echorev);
	}
).store;

/* FM Rhodes Synthesizer (from sccode, with slight adaptions) */
SynthDef(\rhodey_sc_2ch, {
    |
    // standard meanings
	freq = 440, gate = 1, pos = 0, lvl = 0.1,
    // all of these range from 0 to 1
    vel = 0.8, modIndex = 0.2, mix = 0.2, lfoSpeed = 0.8, lfoDepth = 0.07, rev=0.0, echo=0.0, echorev=0.0
    |
    var env1, env2, env3, env4;
    var osc1, osc2, osc3, osc4, snd, outenv, out;

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

    out = Pan2.ar(snd, pos, lvl);

	Out.ar(0, out * (1.0 - rev));
	Out.ar(2, out * rev);
	Out.ar(4, out * echo);
	Out.ar(6, out * echorev);
}).store;

0.exit;