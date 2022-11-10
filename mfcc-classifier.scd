(
~load_audio_file = {
    arg filename, start_frame = 0, num_frames = -1;
    var path = FluidFilesPath(filename);
    Buffer.read(s, path, start_frame, num_frames);
};

~split_frame = 1783040;

~get_training_audio = {
    arg filename;
    ~load_audio_file.value(filename, num_frames: ~split_frame);
};

~get_testing_audio = {
    arg filename;
    ~load_audio_file.value(filename, start_frame: ~split_frame);
};

~trombone_file = "Olencki-TenTromboneLongTones-M.wav";
~oboe_file = "Harker-DS-TenOboeMultiphonics-M.wav";

~display_waveform = {
    arg audio;
    FluidWaveform(audio);
};

~analyze_and_play = {
    arg audio_buffer, number_of_coefficients, mfcc_buffer;
    {
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

~add_point = {
    arg state, timbre_data, mfcc_buffer, labels, label;
    var id = "sample %".format(state[\counter]);
    timbre_data.addPoint(id, mfcc_buffer);
    labels.addLabel(id, label);
    state[\counter] = state[\counter] + 1;
    id;
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
            var audio = PlayBuf.ar(1, audio_buffer, BufRateScale.ir(audio_buffer), doneAction: 2);
            var mfcc = FluidMFCC.kr(audio, ~number_of_coefficients, startCoeff: 1, maxNumCoeffs: ~number_of_coefficients);
            var loudness = FluidLoudness.kr(audio)[0];
            var threshold = -40;
            var is_predicting = loudness >= threshold;
            var trigger = Impulse.kr(30);
            FluidKrToBuf.kr(mfcc, mfcc_buffer);
            neural_network.kr(trigger * is_predicting, mfcc_buffer, predictions);
            SendReply.kr(trigger * is_predicting, "/prediction", FluidBufToKr.kr(predictions));
            SendReply.kr(trigger * (1 - is_predicting), "/prediction", -1);
            audio.dup;
        }.play;
    };

    var predictions = get_predictions_buffer.value;
    set_prediction_action.value;
    play_prediction_synth.value(audio_buffer, mfcc_buffer, neural_network, predictions);
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

~load_audio = {
    ~trombone_training_audio = ~get_training_audio.value(~trombone_file);
    ~trombone_testing_audio = ~get_testing_audio.value(~trombone_file);
    ~oboe_training_audio = ~get_training_audio.value(~oboe_file);
    ~oboe_testing_audio = ~get_testing_audio.value(~oboe_file);
    ~number_of_coefficients = 13;
    ~mfcc_buffer = Buffer.alloc(s, ~number_of_coefficients);
    ~timbre_data = FluidDataSet(s);
    ~labels = FluidLabelSet(s);
    ~state = Dictionary.newFrom([\counter, 0]);

    ~reset = {
        ~state[\counter] = 0;
        [~timbre_data, ~mfcc_buffer, ~labels].do {
            arg data;
            data.clear;
        }
    };
};

~reset_windows = {
    Window.closeAll;
    ServerMeter(s, 2, 2);
};

~set_button = {
    arg parent, bounds, states, action;
    Button(parent, bounds).states_(states).action_(action);
};

~set_add_points_button = {
    arg parent, state, timbre_data, mfcc_buffer, labels, label, bounds;
    var states = [["Add Points"]];
    var add_point = {
            var id = "sample %".format(state[\counter]);
            timbre_data.addPoint(id, mfcc_buffer);
            labels.addLabel(id, label);
            state[\counter] = state[\counter] + 1;
            id;
    };
    ~set_button.value(parent, bounds, states, add_point);
};

~get_window = {
    arg state, timbre_data, mfcc_buffer, labels, label;
    var name = "MLP Regressor";
    var bounds = Rect(10, 10, 840, 320);
    var window = Window(name, bounds).front;
    ~set_add_points_button.value(
        window,
        state,
        timbre_data,
        mfcc_buffer,
        labels,
        label,
        bounds: Rect(10, 10, 100, 20)
    );
};

s.waitForBoot {
    ~load_audio.value;
    ~reset_windows.value;
    ~get_window.value;
};
)

~reset.value;

~analyze_and_play.value(~trombone_training_audio, ~number_of_coefficients, ~mfcc_buffer);
~add_point.value(~state, ~timbre_data, ~mfcc_buffer, ~labels, "trombone");

~analyze_and_play.value(~oboe_training_audio, ~number_of_coefficients, ~mfcc_buffer);
~add_point.value(~state, ~timbre_data, ~mfcc_buffer, ~labels, "oboe");


~train.value;

~make_predictions.value(~trombone_testing_audio, ~mfcc_buffer, ~neural_network);
~make_predictions.value(~oboe_testing_audio, ~mfcc_buffer, ~neural_network);

~analyze_and_play.value(~trombone_testing_audio, ~number_of_coefficients, ~mfcc_buffer);
~analyze_and_play.value(~oboe_testing_audio, ~number_of_coefficients, ~mfcc_buffer);