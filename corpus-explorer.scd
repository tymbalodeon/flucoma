(
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

~get_onset_slices = {
    FluidBufOnsetSlice.processBlocking(
        s,
        ~audio_file,
        metric: 9,
        threshold: 0.5,
        indices: ~indices,
    );
};

~get_spectral_centroid = {
    arg start, number_of_frames, spectral_centroids;
    FluidBufSpectralShape.processBlocking(
        server: s,
        source: ~audio_file,
        startFrame: start,
        numFrames: number_of_frames,
        features: spectral_centroids,
        select: [\centroid]
    );
};

~get_mean_centroid = {
    arg spectral_centroids, stats;
    FluidBufStats.processBlocking(
        server: s,
        source: spectral_centroids,
        stats: stats,
        select: [\mean]
    );
};

~add_to_mean_centroids_array = {
    arg stats, mean_centroids, index;
    FluidBufCompose.processBlocking(
        server: s,
        source: stats,
        destination: mean_centroids,
        destStartFrame: index
    );
};

~get_mean_centroids = {
    arg float_array;
    var spectral_centroids = Buffer(s);
    var stats = Buffer(s);
    var mean_centroids = Buffer(s);
    float_array.doAdjacentPairs{
        arg start, end, index;
        var number_of_frames = end - start;
        ~get_spectral_centroid.(start, number_of_frames, spectral_centroids);
        ~get_mean_centroid.(spectral_centroids, stats);
        ~add_to_mean_centroids_array.(stats, mean_centroids, index);
    };
    s.sync;
    mean_centroids.loadToFloatArray(
        action: {
            arg float_array;
            float_array.do {
                arg slice_index, count_index;
                var count = count_index + 1;
                "Slice %: %".format(count, slice_index).postln;
            };
            "Found % slices.".format(float_array.size).postln;
            ~mean_centroids = float_array;
        };
    );
};

~get_slices = {
    ~indices = Buffer(s);
    ~get_onset_slices.();
    ~indices.loadToFloatArray(action: ~get_mean_centroids);
};

~display_slices = {
    FluidWaveform(~audio_file, ~indices);
};

~play_slice = {
    arg index;
    {
        var start = Index.kr(~indices, index);
        var stop = Index.kr(~indices, index + 1);
        var phasor = Phasor.ar(0, BufRateScale.ir(~audio_file), start, stop);
        var audio_file = BufRd.ar(1, ~audio_file, phasor);
        var duration = (stop - start) / BufSampleRate.ir(~audio_file);
        var fade_time = 0.03;
        var total_fade_time = fade_time * 2;
        var non_fade_time = duration - total_fade_time;
        var env = Env([0, 1, 1, 0], [fade_time, non_fade_time, fade_time]);
        var env_gen = EnvGen.kr(env, doneAction: 2);
        audio_file.dup * env_gen;
    }.play;
};

~play_slices = {
    fork {
        ~mean_centroids.order.do {
            arg index;
            "Playing Slice %...".format(index).postln;
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
    ~display_slices.();
};
)