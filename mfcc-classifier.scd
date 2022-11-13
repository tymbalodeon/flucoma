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
    arg audio_buffer, number_of_coefficients, mfcc_buffer, type;
    ~state[type] = {
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
    arg type;
    ~state[type].free;
};

~trombone_file = "Olencki-TenTromboneLongTones-M.wav";
~oboe_file = "Harker-DS-TenOboeMultiphonics-M.wav";

~get_trombone_training_audio = {
    ~get_training_audio.value(~trombone_file);
};

~get_trombone_testing_audio = {
    ~get_testing_audio.value(~trombone_file);
};

~get_oboe_training_audio = {
    ~get_training_audio.value(~oboe_file);
};

~get_oboe_testing_audio = {
    ~get_testing_audio.value(~oboe_file);
};

~set_play_button = {
    arg window, audio_buffer, instrument_name, bounds, type;
    var states = [
        ["Play % %".format(instrument_name, type)],
        ["Stop % %".format(instrument_name, type)]
    ];
    var play_and_stop = {
        arg button;
        var playing = button.value;
        if (playing == 0) {
            "% % stopped.".format(instrument_name, type).postln;
            ~stop_analyzing_and_playing.value(type);
        } {
            "Playing % %...".format(instrument_name, type).postln;
            ~analyze_and_play.value(
                audio_buffer,
                ~number_of_coefficients,
                ~mfcc_buffer,
                type
            );
        };
    };
    ~set_button.value(window, bounds, states, play_and_stop);
};

~reset = {
    ~state[\counter] = 0;
    [~timbre_data, ~mfcc_buffer, ~labels].do {
        arg data;
        data.clear;
    };
    "Points reset.".postln;
};

~set_reset_button = {
    arg window, bounds;
    ~set_button.value(
        window,
        bounds,
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

~train =  {
    20.do {
        ~train_neural_network.value(~neural_network);
    };
};

~set_train_button = {
    arg window, bounds;
    ~set_button.value(
        window,
        bounds,
        [["Train"]],
        ~train
    );
};

~get_predictions_buffer = {
    var num_frames = 1;
    Buffer.alloc(s, num_frames);
};

~set_prediction_action = {
    var post_message = {
        arg message;
        message[3].postln;
    };
    OSCdef(\predictions, post_message, "/prediction");
};

~play_prediction_synth = {
    arg mfcc_buffer, neural_network, predictions;
    // TODO fix the following to use the buffer, not the synth
    var audio_buffer = ~state[\testing];
    ~state[\predictions] = {
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

~stop_prediction_synth = {
    ~state[\predictions].free;
};

~make_predictions = {
    arg mfcc_buffer, neural_network;
    var predictions = ~get_predictions_buffer.value;
    ~set_prediction_action.value;
    ~play_prediction_synth.value(
        mfcc_buffer,
        neural_network,
        predictions
    );
};

~set_predictions_button = {
    arg window, bounds, mfcc_buffer, neural_network;
    var states = [["Make predictions"], ["Stop predicitons"]];
    var play_and_stop_predictions = {
        arg button;
        var playing = button.value;
        if (playing == 0) {
            "Predictions stopped.".postln;
            ~stop_prediction_synth.value();
        } {
            "Playing predictions...".postln;
            ~make_predictions.value(mfcc_buffer, neural_network);
        };
    };
    ~set_button.value(window, bounds, states, play_and_stop_predictions);
};

~get_window = {
    arg state, timbre_data, mfcc_buffer, labels, neural_network;
    var name = "MFCC Classifier";
    var window = Window(name).front;
    ~set_add_points_button.value(
        window,
        Rect(50, 50, 200, 20),
        state,
        timbre_data,
        mfcc_buffer,
        labels
    );
    ~set_play_button.value(
        window,
        ~get_trombone_training_audio.value,
        \Trombone,
        Rect(50, 100, 200, 20),
        \training
    );
    ~set_play_button.value(
        window,
        ~get_oboe_training_audio.value,
        \Oboe,
        Rect(50, 150, 200, 20),
        \training
    );
    ~set_reset_button.value(window, Rect(350, 50, 200, 20));
    ~set_train_button.value(window, Rect(350, 100, 200, 20));
    ~set_play_button.value(
        window,
        ~get_trombone_testing_audio.value,
        \Trombone,
        Rect(50, 200, 200, 20),
        \testing
    );
    ~set_play_button.value(
        window,
        ~get_oboe_testing_audio.value,
        \Oboe,
        Rect(50, 250, 200, 20),
        \testing
    );
    ~set_predictions_button.value(
        window,
        Rect(350, 150, 200, 20),
        mfcc_buffer,
        neural_network
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

~load_mfcc = {
    ~reset_windows.value;
    ~state = Dictionary.newFrom([\counter, 0]);
    ~timbre_data = FluidDataSet(s);
    ~number_of_coefficients = 13;
    ~mfcc_buffer = Buffer.alloc(s, ~number_of_coefficients);
    ~labels = FluidLabelSet(s);
    ~neural_network = ~get_neural_network.value;
    ~get_window.value(
        ~state,
        ~timbre_data,
        ~mfcc_buffer,
        ~labels,
        ~neural_network
    );
};

s.waitForBoot {
    ~load_mfcc.value;
};
)