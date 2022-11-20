(
~get_dest_gain = {
    arg channel;
    if (channel == 0) {
        0;
    } {
        1;
    };
};

~convert_to_mono = {
    arg buffer;
    var channels = buffer.numChannels;
    channels.do {
        arg channel;
        FluidBufCompose.processBlocking(
            source: buffer,
            startChan: channel,
            numChans: 1,
            destination: ~audio_file,
            destStartChan: 0,
            gain: -6.dbamp,
            destGain: ~get_dest_gain.(channel)
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
    arg threshold;
    FluidBufOnsetSlice.processBlocking(
        source: ~audio_file,
        metric: 9,
        threshold: threshold,
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

~get_mfcc = {
    arg start, number_of_frames, mfcc;
    FluidBufMFCC.processBlocking(
        source: ~audio_file,
        startFrame: start,
        numFrames: number_of_frames,
        features: mfcc,
        startCoeff: 1
    );
};

~get_mean_mfcc = {
    arg mfcc, stats;
    FluidBufStats.processBlocking(
        source: mfcc,
        stats: stats,
        select: [\mean]
    );
};

~flatten_mean_mfcc = {
    arg stats, flattened_mean_mfcc;
    FluidBufFlatten.processBlocking(
        source: stats,
        destination: flattened_mean_mfcc
    );
};

~make_kd_tree = {
    var umapped = FluidDataSet();
    var umapper = FluidUMAP(minDist: 0.5);
    umapper.fitTransform(~analyses, umapped);
    ~normalized = FluidDataSet();
    FluidNormalize().fitTransform(umapped, ~normalized);
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
    var mfcc = Buffer();
    var stats = Buffer();
    var flattened_mean_mfcc = Buffer();
    "Analyzing slices...".postln;
    fork {
        float_array.doAdjacentPairs {
            arg start, end, index;
            var number_of_frames = end - start;
            var slice_number = index + 1;
            ~get_mfcc.(start, number_of_frames, mfcc);
            ~get_mean_mfcc.(mfcc, stats);
            ~flatten_mean_mfcc.(stats, flattened_mean_mfcc);
            ~analyses.addPoint(index, flattened_mean_mfcc);
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
    arg input_folder, threshold;
    ~load_input_folder.(input_folder);
    ~indices = Buffer();
    ~analyses = FluidDataSet();
    ~get_onset_slices.(threshold);
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
    arg input_folder, threshold = 0.05;
    "helpers.scd".load;
    ~reset.();
    ~open_plotter.(input_folder, threshold);

};

s.waitForBoot {
    var input_folder = FluidFilesPath();
    ~main.(input_folder);
};
)