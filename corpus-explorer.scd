(
~audio_file = Buffer.read(s, FluidFilesPath("Nicol-LoopE-M.wav"));

{
    var sig = PlayBuf.ar(1, ~audio_file, BufRateScale.ir(~audio_file), loop: 1);
    var slices = FluidOnsetSlice.ar(sig, 9, 0.5);
    [sig, slices];
}.play;

~indices = Buffer(s);
FluidBufOnsetSlice.processBlocking(
    s,
    ~audio_file,
    metric: 9,
    threshold: 0.5,
    indices: ~indices,
);

~indices.loadToFloatArray(
    action: {
        arg float_array;
        float_array.postln;
        float_array.size.postln;
    };
);

// FluidWaveform(~audio_file, ~indices);

~play_slice = {
    arg index;
    {
        var start_sample = Index.kr(~indices, index);
        var stop_sample = Index.kr(~indices, index + 1);
        var phasor = Phasor.ar(
            0,
            BufRateScale.ir(~audio_file),
            start_sample,
            stop_sample
        );
        var signal = BufRd.ar(1, ~audio_file, phasor);
        var duration = {
            (stop_sample - start_sample) / BufSampleRate.ir(~audio_file);
        }.();
        var env = Env([0 , 1, 1, 0], [0.03, duration - 0.06, 0.03]);
        var env_gen = EnvGen.kr(env, doneAction: 2);
        signal.dup * env_gen;
    }.play;
};

fork {
    (~indices.numFrames - 1).do {
        arg index;
        ~play_slice.(index);
        1.wait;
    };
};
)