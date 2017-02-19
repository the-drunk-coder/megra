// the typical filtered grain player, for two channels ...
SynthDef(\grain_2ch, {| bufnum=0, lvl=0.5, rate=1.0, start=0.0, lp_freq=19000, lp_q=0.1, lp_dist=0.0, pf_freq=4000, pf_q=0.1, pf_gain=0.0, hp_freq=20, hp_q=0.3, a=0.01, length=1, r=0.02, pos=0.0 |
	var lim_gain = min(lvl, 1.0);
	var cutenv = Env.new([0, lim_gain, lim_gain, 0], [a, length, r]);
	var sample = PlayBuf.ar(1, bufnum, rate, 1, startPos: BufDur.ir(bufnum) * 44100 * start, loop: 1);
	var sample_hp = BHiPass.ar(sample, hp_freq, hp_q);
	var sample_peak = BPeakEQ.ar(sample_hp, pf_freq, pf_q, pf_gain);
	var sample_lp = LPF18.ar(sample_peak, lp_freq, lp_q, lp_dist);
	var sample_env = Limiter.ar(sample_lp, level:1.0, dur:0.006);
	Out.ar(0, Pan2.ar(sample_env * EnvGen.ar(cutenv, doneAction:2), pos));
}).store;


