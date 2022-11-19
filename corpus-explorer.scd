(
~play_with_slices = {
    {
        var audio = PlayBuf.ar(
            numChannels: 1,
            bufnum: ~audio_file,
            rate: BufRateScale.ir(~audio_file),
            doneAction: 2
        );
        var slices = FluidOnsetSlice.ar(
            in: audio,
            metric: \rcomplex,
            threshold: 0.5
        );
        [audio, slices];
    }.play;
};

~get_onset_slices = {
    FluidBufOnsetSlice.processBlocking(
        server: s,
        source: ~audio_file,
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

~get_loudness = {
    arg start, number_of_frames, loudness;
    FluidBufLoudness.processBlocking(
        server: s,
        source: ~audio_file,
        startFrame: start,
        numFrames: number_of_frames,
        features: loudness,
        select: [\loudness]
    )
};

~get_mean_loudness = {
    arg loudness, stats;
    FluidBufStats.processBlocking(
        server: s,
        source: loudness,
        stats: stats,
        select: [\mean]
    );
};

~add_slice_data_to_analyses = {
    arg stats, loudness_stats, point;
    [stats, loudness_stats].do {
        arg stats_data, index;
        FluidBufCompose.processBlocking(
            server: s,
            source: stats_data,
            destination: point,
            destStartFrame: index
        );
    };
};

~get_mean_centroids = {
    arg float_array;
    var spectral_centroids = Buffer(s);
    var stats = Buffer(s);
    var loudness = Buffer(s);
    var loudness_stats = Buffer(s);
    var point = Buffer(s);
    float_array.doAdjacentPairs {
        arg start, end, index;
        var number_of_frames = end - start;
        ~get_spectral_centroid.(start, number_of_frames, spectral_centroids);
        ~get_mean_centroid.(spectral_centroids, stats);
        ~get_loudness.(start, number_of_frames, loudness);
        ~get_mean_loudness.(loudness, loudness_stats);
        ~add_slice_data_to_analyses.(stats, loudness_stats, point);
        ~analyses.addPoint(index, point);
    };
    s.sync;
    ~analyses.print;
};

~make_slices = {
    ~indices = Buffer(s);
    ~analyses = FluidDataSet(s);
    ~get_onset_slices.();
    ~indices.loadToFloatArray(action: ~get_mean_centroids);
};

~make_kd_tree = {
    ~normalized = FluidDataSet(s);
    FluidNormalize(s).fitTransform(~analyses, ~normalized);
    ~kd_tree = FluidKDTree(s);
    ~kd_tree.fit(~normalized);
};

~find_nearest_point = {
    arg view, x, y;
    ~point.setn(0, [x, y]);
    ~kd_tree.kNearest(
        ~point,
        k: 1,
        action: {
            arg nearest_point;
            if (nearest_point != ~previous_point) {
                view.highlight_(nearest_point);
                ~play_slice.(nearest_point.asInteger);
                ~previous_point = nearest_point;
            };
        }
    );
};

~open_plotter = {
    ~normalized.dump {
        arg dictionary;
        ~point = Buffer.alloc(s, 2);
        ~previous_point = nil;
        defer {
            FluidPlotter(
                dict: dictionary,
                mouseMoveAction: ~find_nearest_point
            );
        };
    };
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

s.quit;
s.waitForBoot {
    "helpers.scd".load;
    ~reset_windows.();
    ~audio_file = Buffer.read(s, FluidFilesPath("Nicol-LoopE-M.wav"));
    ~make_slices.();
    ~make_kd_tree.();
    ~open_plotter.();
};
)
