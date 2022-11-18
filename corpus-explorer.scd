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

~analyze_audio = {
    ~indices = Buffer(s);
    FluidBufOnsetSlice.processBlocking(
        s,
        ~audio_file,
        metric: 9,
        threshold: 0.5,
        indices: ~indices,
    );
};

~get_slices = {
    ~analyze_audio.();
    ~indices.loadToFloatArray(
        action: {
            arg float_array;
            var spectral_centroid = Buffer(s);
            var stats = Buffer(s);
            var mean_centroids = Buffer(s);
            float_array.doAdjacentPairs{
                arg start, end, index;
                var number_of_frames = end - start;
                FluidBufSpectralShape.processBlocking(
                    s,
                    ~audio_file,
                    start,
                    number_of_frames,
                    features: spectral_centroid,
                    select: [\centroid]
                );
                FluidBufStats.processBlocking(
                    s,
                    spectral_centroid,
                    stats: stats,
                    select: [\mean]
                );
                FluidBufCompose.processBlocking(
                    s,
                    stats,
                    destination: mean_centroids,
                    destStartFrame:
                    index
                );
            };
            s.sync;
            mean_centroids.loadToFloatArray(
                action: {
                    arg float_array;
                    float_array.do({
                        arg slice_index, count_index;
                        var count = count_index + 1;
                        "Slice %: %".format(count, slice_index).postln;
                    });
                    "Found % slices.".format(float_array.size).postln;
                    ~mean_centroids = float_array;
                };
            );
        };
    );
};

~display_slices = {
    FluidWaveform(~audio_file, ~indices);
};

~play_slice = {
    arg index, amplitude = 0.1;
    {
        var rate = BufRateScale.ir(~audio_file);
        var start = Index.kr(~indices, index);
        var stop = Index.kr(~indices, index + 1);
        var phasor = Phasor.ar(0, rate, start, stop);
        var audio = BufRd.ar(1, ~audio_file, phasor);
        var duration = (stop - start) / rate;
        var fade_time = 0.03;
        var total_fade_time = fade_time * 2;
        var env = Env(
            [0, 1, 1, 0],
            [fade_time, duration - total_fade_time, fade_time]
        );
        var env_gen = EnvGen.kr(env, doneAction: 2);
        audio.dup * env_gen * amplitude;
    }.play;
};

~play_slices = {
    fork {
        ~mean_centroids.order.do {
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
    ~display_slices.();
};