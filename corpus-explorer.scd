(
~convert_to_mono = {
    arg buffer;
    var channels = buffer.numChannels;
    channels.do {
        arg channel;
        var dest_gain = {
            if (channel == 0) {
                0;
            } {
                1;
            };
        };
        FluidBufCompose.processBlocking(
            server: s,
            source: buffer,
            startChan: channel,
            numChans: 1,
            destination: ~audio_file,
            destStartChan: 0,
            gain: -6.dbamp,
            destGain: dest_gain
        );
    };
};

~load_input_folder = {
    arg input_folder;
    var folder = FluidLoadFolder(input_folder).play;
    var buffer = folder.buffer;
    if (buffer.numChannels > 1) {
        ~audio_file = Buffer();
        ~convert_to_mono.(buffer);
    } {
        ~audio_file = buffer;
    };
};

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
        threshold: 0.05,
        indices: ~indices,
        action: {
            var duration = ~audio_file.duration;
            var number_of_frames = ~indices.numFrames;
            var average_seconds_per_slice = duration / number_of_frames;
            "Average seconds per slice: %".format(
                average_seconds_per_slice
            ).postln;
        }
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
        select: [\high]
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

~make_kd_tree = {
    ~normalized = FluidDataSet();
    FluidNormalize().fitTransform(~analyses, ~normalized);
    ~kd_tree = FluidKDTree();
    ~kd_tree.fit(~normalized);
};

~play_slice = {
    arg index;
    {
        var start = Index.kr(~indices, index);
        var stop = Index.kr(~indices, index + 1);
        var phasor = Phasor.ar(0, BufRateScale.ir(~audio_file), start, stop);
        var audio = BufRd.ar(1, ~audio_file, phasor);
        var total_duration = (stop - start) / BufSampleRate.ir(~audio_file);
        var duration = min(total_duration, 1);
        var fade_time = 0.03;
        var total_fade_time = fade_time * 2;
        var non_fade_time = duration - total_fade_time;
        var env = Env([0, 1, 1, 0], [fade_time, non_fade_time, fade_time]);
        var env_gen = EnvGen.kr(env, doneAction: 2);
        audio.dup * env_gen;
    }.play;
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

~make_plotter = {
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

~analyze_and_plot = {
    arg float_array;
    var spectral_centroids = Buffer();
    var stats = Buffer();
    var loudness = Buffer();
    var loudness_stats = Buffer();
    var point = Buffer();
    "Analyzing slices...".postln;
    fork {
        float_array.doAdjacentPairs {
            arg start, end, index;
            var number_of_frames = end - start;
            var slice_number = index + 1;
            ~get_spectral_centroid.(start, number_of_frames, spectral_centroids);
            ~get_mean_centroid.(spectral_centroids, stats);
            ~get_loudness.(start, number_of_frames, loudness);
            ~get_mean_loudness.(loudness, loudness_stats);
            ~add_slice_data_to_analyses.(stats, loudness_stats, point);
            ~analyses.addPoint(index, point);
            if ((slice_number % 100) == 99) {
                s.sync;
            };
        };
        s.sync;
        ~analyses.print;
        ~make_kd_tree.();
        ~make_plotter.();
    };
};

~open_plotter = {
    arg input_folder;
    ~load_input_folder.(input_folder);
    ~indices = Buffer();
    ~analyses = FluidDataSet();
    ~get_onset_slices.();
    ~indices.loadToFloatArray(action: ~analyze_and_plot);
};

~display_slices = {
    FluidWaveform(~audio_file, ~indices);
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

~main = {
    arg input_folder;
    "helpers.scd".load;
    ~reset.();
    ~open_plotter.(input_folder);

};

s.waitForBoot {
    var input_folder = FluidFilesPath();
    ~main.(input_folder);
};
)