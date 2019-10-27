SynthDef(\reverb_8ch, {
	var in1, in2, in3, in4;
	var out1, out2, out3, out4;
	# in1, in2, in3, in4 = In.ar(4, 4);

	out1 = PartConv.ar(in1, 2048, 1, 1.0);
	out2 = PartConv.ar(in2, 2048, 1, 1.0);
	out3 = PartConv.ar(in3, 2048, 1, 1.0);
	out4 = PartConv.ar(in4, 2048, 1, 1.0);

	Out.ar(0, [out1, out2, out3, out4]);
}).store;

SynthDef(\grain_4ch, {

	|
	bufnum=0, lvl=0.5, rate=1.0, start=0.0,

	//lowpass
	lp_freq=19000, lp_q=0.1, lp_dist=0.0, lp_freq_lfo_freq=0.0, lp_freq_lfo_depth = 0.0, lp_freq_lfo_phase = 0.0,

	// peak
	pf_freq=4000, pf_q=0.1, pf_gain=0.0,

	// hi-pass
	hp_freq=20, hp_q=0.3,

	// env
	a=0.01, length=1, r=0.02, pos=0.0, rev=0.0
	|

	var lim_gain = min(lvl, 1.0);
	var cutenv = Env.new([0, lim_gain, lim_gain, 0], [a, length, r]);
	var outenv = Env.new([0, lim_gain, lim_gain, 0], [a, length + 1.5, r]);
	var sample = PlayBuf.ar(1, bufnum, rate, 1, startPos: BufDur.ir(bufnum) * 44100 * start, loop: 1) * EnvGen.ar(cutenv);
	var sample_hp = BHiPass.ar(sample, hp_freq, hp_q);
	var sample_peak = BPeakEQ.ar(sample_hp, pf_freq, pf_q, pf_gain);
	var sample_lp = LPF18.ar(sample_peak, lp_freq - ((lp_freq * lp_freq_lfo_depth) * SinOsc.ar(lp_freq_lfo_freq, lp_freq_lfo_phase)), lp_q, lp_dist);
	var out = PanAz.ar(4, sample_lp * EnvGen.ar(cutenv, doneAction:2), pos * 0.5, orientation: 0);
	Out.ar(0, out * (1.0 - rev));
	Out.ar(4, out * rev);
}).store;

0.exit;