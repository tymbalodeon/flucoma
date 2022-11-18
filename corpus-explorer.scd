~play_with_slices = {
    {
        var audio = PlayBuf.ar(
            1,
            ~audio_file,
            BufRateScale.ir(~audio_file),
            doneAction: 2
        );
        var slices = FluidOnsetSlice.ar(audio, 9, 0.5);
        [audio, slices];
    }.play;
};

~display_waveform = {
    FluidWaveform(~audio_file, ~indices);
};

~get_slices = {
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
            float_array.do({
                arg slice_index, count_index;
                var count = count_index + 1;
                "Slice %: %".format(count, slice_index).postln;
            });
            "Found % slices.".format(float_array.size).postln;
        };
    );
};

~play_slice = {
    arg index, amplitude = 0.1;
    {
        var rate = BufRateScale.ir(~audio_file);
        var start_sample = Index.kr(~indices, index);
        var stop_sample = Index.kr(~indices, index + 1);
        var phasor = Phasor.ar(0, rate, start_sample, stop_sample);
        var audio = BufRd.ar(1, ~audio_file, phasor);
        var duration = (stop_sample - start_sample) / rate;
        var env = Env([0 , 1, 1, 0], [0.03, duration - 0.06, 0.03]);
        var env_gen = EnvGen.kr(env, doneAction: 2);
        audio.dup * env_gen * amplitude;
    }.play;
};

~play_slices = {
    fork {
        (~indices.numFrames - 1).do {
            arg index;
            ~play_slice.(index);
            1.wait;
        };
    };
};

s.waitForBoot {
    "helpers.scd".load;
    ~reset_windows.();
    ~audio_file = Buffer.read(s, FluidFilesPath("Nicol-LoopE-M.wav"));
    ~get_slices.();
    ~play_slices.();
};