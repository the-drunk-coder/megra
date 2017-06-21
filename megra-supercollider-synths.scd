// reverb buffer hardcoded to 1

// the typical filtered grain player, for two channels ...
SynthDef(\grain_2ch, {| bufnum=0, rev_bufnum=0, lvl=0.5, rate=1.0, start=0.0, lp_freq=19000,
	                    lp_q=0.1, lp_dist=0.0, pf_freq=4000, pf_q=0.1, pf_gain=0.0,
	                    hp_freq=20, hp_q=0.3, a=0.01, length=1, r=0.02, pos=0.0 |
	var lim_gain = min(lvl, 1.0);
	var cutenv = Env.new([0, lim_gain, lim_gain, 0], [a, length, r]);
	var outenv = Env.new([0, lim_gain, lim_gain, 0], [a, length + 2.5, r]);
	var sample = PlayBuf.ar(1, bufnum, rate, 1, startPos: BufDur.ir(bufnum) * 44100 * start, loop: 1) * EnvGen.ar(cutenv);
	var sample_hp = BHiPass.ar(sample, hp_freq, hp_q);
	var sample_peak = BPeakEQ.ar(sample_hp, pf_freq, pf_q, pf_gain);
	var sample_lp = LPF18.ar(sample_peak, lp_freq, lp_q, lp_dist);
	Out.ar(0, Pan2.ar(sample_lp * EnvGen.ar(outenv, doneAction:2), pos));
}).store;

// the typical filtered grain player, for two channels, with convolution reverb
SynthDef(\grain_2ch_rev, {| bufnum=0, lvl=0.5, rate=1.0, start=0.0, lp_freq=19000, lp_q=0.1,
	                        lp_dist=0.0, pf_freq=4000, pf_q=0.1, pf_gain=0.0, hp_freq=20,
	                        hp_q=0.3, a=0.01, length=1, r=0.02, pos=0.0, rev=0.0 |
	var lim_gain = min(lvl, 1.0);
	var cutenv = Env.new([0, lim_gain, lim_gain, 0], [a, length, r]);
	var sample = PlayBuf.ar(1, bufnum, rate, 1, startPos: BufDur.ir(bufnum) * 44100 * start, loop: 1) * EnvGen.ar(cutenv);
	var sample_hp = BHiPass.ar(sample, hp_freq, hp_q);
	var sample_peak = BPeakEQ.ar(sample_hp, pf_freq, pf_q, pf_gain);
	var sample_lp = LPF18.ar(sample_peak, lp_freq, lp_q, lp_dist);
	var sample_rev = PartConv.ar(sample_lp, 2048, 1, rev);
	var sample_out = (sample_lp * (1.0 - rev)) + sample_rev;
	var revenv = Env.new([0, lim_gain, lim_gain, 0], [a, length + 2.5, r]);
	Out.ar(0, Pan2.ar(sample_out *  EnvGen.ar(revenv, doneAction:2) , pos));
}).store;