(
~reset_windows = {
    Window.closeAll;
    ServerMeter(s, 2, 2);
};

~display_waveform = {
    arg audio;
    FluidWaveform(audio);
};

~split_frame = 1783040;

~load_audio_file = {
    arg filename, start_frame = 0, num_frames = -1;
    var path = FluidFilesPath(filename);
    Buffer.read(s, path, start_frame, num_frames);
};

~get_training_audio = {
    arg filename;
    ~load_audio_file.value(filename, num_frames: ~split_frame);
};

~get_testing_audio = {
    arg filename;
    ~load_audio_file.value(filename, start_frame: ~split_frame);
};

~set_button = {
    arg window, bounds, states, action;
    Button(window, bounds).states_(states).action_(action);
};

~set_add_points_button = {
    arg window, bounds, state, timbre_data, mfcc_buffer, labels;
    var states = [["Add Points"]];
    var add_point = {
        var id = "Sample %".format(state[\counter]);
        timbre_data.addPoint(id, mfcc_buffer);
        labels.addLabel(id, "label");
        state[\counter] = state[\counter] + 1;
        "Added %.".format(id).postln;
        id;
    };
    ~set_button.value(window, bounds, states, add_point);
};

~analyze_and_play = {
    arg audio_buffer, number_of_coefficients,
    mfcc_buffer, instrument_name;
    ~state[instrument_name] = {
        var audio = PlayBuf.ar(
            numChannels: 1,
            bufnum: audio_buffer,
            rate: BufRateScale.ir(audio_buffer),
            doneAction: 2
        );
        var mfcc = FluidMFCC.kr(
            audio,
            startCoeff: 1,
            maxNumCoeffs: number_of_coefficients
        );
        FluidKrToBuf.kr(mfcc, mfcc_buffer);
        audio.dup;
    }.play;
};

~stop_analyzing_and_playing = {
    arg instrument_name;
    ~state[instrument_name].free;
};

~set_play_button = {
    arg window, audio_buffer, instrument_name, bounds;
    var states = [
        ["Play %".format(instrument_name)],
        ["Stop %".format(instrument_name)]
    ];
    var analyze_and_play = {
        arg button;
        var playing = button.value;
        if (playing == 0) {
            "% stopped.".format(instrument_name).postln;
            ~stop_analyzing_and_playing.value(instrument_name);
        } {
            "Playing %...".format(instrument_name).postln;
            ~analyze_and_play.value(
                audio_buffer,
                ~number_of_coefficients,
                ~mfcc_buffer,
                instrument_name
            );
        };
    };
    ~set_button.value(window, bounds, states, analyze_and_play);
};

~get_trombone_training_audio = {
    ~trombone_file = "Olencki-TenTromboneLongTones-M.wav";
    ~get_training_audio.value(~trombone_file);
};

~get_oboe_training_audio = {
    ~oboe_file = "Harker-DS-TenOboeMultiphonics-M.wav";
    ~get_training_audio.value(~oboe_file);
};

~reset = {
    ~state[\counter] = 0;
    [~timbre_data, ~mfcc_buffer, ~labels].do {
        arg data;
        data.clear;
    };
    "Points reset.".postln;
};

~get_window = {
    arg state, timbre_data, mfcc_buffer, labels;
    var name = "MFCC Classifier";
    var window = Window(name).front;
    ~set_add_points_button.value(
        window,
        Rect(50, 50, 100, 20),
        state,
        timbre_data,
        mfcc_buffer,
        labels
    );
    ~set_play_button.value(
        window,
        ~get_trombone_training_audio.value,
        \Trombone,
        bounds: Rect(50, 100, 100, 20)
    );
    ~set_play_button.value(
        window,
        ~get_oboe_training_audio.value,
        \Oboe,
        bounds: Rect(50, 150, 100, 20)
    );
    ~set_button.value(
        window,
        Rect(50, 200, 100, 20),
        [["Reset"]],
        ~reset
    );
};

~get_neural_network = {
    var activation = FluidMLPRegressor.sigmoid;
    ~neural_network = FluidMLPClassifier(
        s,
        hiddenLayers: [5],
        activation: activation,
        maxIter: 1000,
        learnRate: 0.1,
        momentum: 0.9,
        batchSize: 5,
        validation: 0
    );
};

~train_neural_network = {
    arg neural_network;
    var post_loss = {
        arg loss;
        loss.postln;
    };
    neural_network.fit(~timbre_data, ~labels, post_loss);
};

~make_predictions = {
    arg audio_buffer, mfcc_buffer, neural_network;

    var get_predictions_buffer = {
        var num_frames = 1;
        Buffer.alloc(s, num_frames);
    };

    var set_prediction_action = {
        var post_message = {
            arg message;
            message[3].postln;
        };
        OSCdef(\predictions, post_message, "/prediction");
    };

    var play_prediction_synth = {
        arg audio_buffer, mfcc_buffer, neural_network, predictions;
        {
            var audio = PlayBuf.ar(
                1,
                audio_buffer,
                BufRateScale.ir(audio_buffer),
                doneAction: 2
            );
            var mfcc = FluidMFCC.kr(
                audio,
                ~number_of_coefficients,
                startCoeff: 1,
                maxNumCoeffs: ~number_of_coefficients
            );
            var loudness = FluidLoudness.kr(audio)[0];
            var threshold = -40;
            var is_predicting = loudness >= threshold;
            var trigger = Impulse.kr(30);
            FluidKrToBuf.kr(mfcc, mfcc_buffer);
            neural_network.kr(
                trigger * is_predicting,
                mfcc_buffer,
                predictions
            );
            SendReply.kr(
                trigger * is_predicting,
                "/prediction",
                FluidBufToKr.kr(predictions)
            );
            SendReply.kr(trigger * (1 - is_predicting), "/prediction", -1);
            audio.dup;
        }.play;
    };

    var predictions = get_predictions_buffer.value;
    set_prediction_action.value;
    play_prediction_synth.value(
        audio_buffer,
        mfcc_buffer,
        neural_network,
        predictions
    );
};

~predict_point = {
    arg neural_network, mfcc_buffer;
    var post_label = {
        arg label;
        label.postln;
    };
    neural_network.predictPoint(mfcc_buffer, post_label);
};

~train =  {
    arg neural_network;
    20.do {
        ~train_neural_network.value(neural_network);
    };
};

~load_mfcc = {
    ~reset_windows.value;
    ~state = Dictionary.newFrom([\counter, 0]);
    ~timbre_data = FluidDataSet(s);
    ~number_of_coefficients = 13;
    ~mfcc_buffer = Buffer.alloc(s, ~number_of_coefficients);
    ~labels = FluidLabelSet(s);
    ~get_window.value(~state, ~timbre_data, ~mfcc_buffer, ~labels);
};

s.waitForBoot {
    ~load_mfcc.value;
};
)