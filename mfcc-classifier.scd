(
~get_neural_network = {
    var activation = FluidMLPRegressor.sigmoid;
    FluidMLPClassifier(
        hiddenLayers: [5],
        activation: activation,
        maxIter: 1000,
        learnRate: 0.1,
        momentum: 0.9,
        batchSize: 5,
        validation: 0
    );
};

~set_add_points_button = {
    arg bounds;
    var states = [["Add point"]];
    var add_point = {
        var id = "Sample %".format(~counter);
        var label = ~instrument_name;
        ~timbre_data.addPoint(id, ~mfcc_buffer);
        ~labels.addLabel(id, label);
        ~counter = ~counter + 1;
        "Added % %.".format(label, id).postln;
    };
    ~counter = 0;
    ~timbre_data = FluidDataSet();
    ~number_of_coefficients = 13;
    ~mfcc_buffer = Buffer.alloc(numFrames: ~number_of_coefficients);
    ~labels = FluidLabelSet();
    ~set_button.(bounds, states, add_point);
};

~get_audio = {
    PlayBuf.ar(
        numChannels: 1,
        bufnum: ~audio_buffer,
        rate: BufRateScale.ir(~audio_buffer),
        doneAction: 2
    );
};

~get_mfcc = {
    arg audio;
    FluidMFCC.kr(
        audio,
        startCoeff: 1,
        maxNumCoeffs: ~number_of_coefficients
    );
};

~set_predictions_synth = {
    arg audio, predictions;
    var loudness = FluidLoudness.kr(audio)[0];
    var threshold = -40;
    var is_predicting = loudness >= threshold;
    var trigger = Impulse.kr(30);
    ~neural_network.kr(trigger * is_predicting, ~mfcc_buffer, predictions);
    SendReply.kr(
        trigger * is_predicting,
        "/prediction",
        FluidBufToKr.kr(predictions)
    );
    SendReply.kr(trigger * (1 - is_predicting), "/prediction", -1);
};

~get_synth = {
    arg make_predictions = false, predictions = nil;
    var audio = ~get_audio.();
    var mfcc = ~get_mfcc.(audio);
    if (make_predictions == true) {
        ~set_predictions_synth.(audio, predictions);
    };
    FluidKrToBuf.kr(mfcc, ~mfcc_buffer);
    audio.dup;
};

~display_waveform = {
    FluidWaveform(~audio_buffer);
};

~analyze_and_play = {
    arg instrument_name;
    ~waveform = ~display_waveform.();
    ~instrument_name = instrument_name;
    ~synth = {
        ~get_synth.();
    }.play;
};

~set_play_button = {
    arg bounds, audio_buffer, instrument_name, testing = false;
    var type = {
        if (testing == true) {
            "testing";
        } {
            "training";
        };
    }.();
    var states = [
        ["Play % %".format(instrument_name, type), ~background, ~green]
    ];
    var play = {
        if (~synth == nil) {
            "Playing % %...".format(instrument_name, type).postln;
            ~audio_buffer = audio_buffer;
            ~analyze_and_play.(
                instrument_name
            );
        };
    };
    ~set_button.(bounds, states, play);
};

~set_stop_playing_button = {
    arg bounds;
    var states = [["Stop", ~foreground, ~red]];
    var stop_analyzing_and_playing = {
        if (~synth != nil) {
            ~synth.free;
            "Playing stopped.".postln;
        };
        if (~waveform != nil) {
            ~waveform.close;
        };
        ~synth = nil;
        ~waveform = nil;
        ~audio_buffer = nil;
        ~instrument_name = nil;
    };
    ~set_button.(bounds, states, stop_analyzing_and_playing);
};

~set_reset_button = {
    arg bounds;
    var states = [["Reset"]];
    var reset = {
        ~counter = 0;
        [~timbre_data, ~mfcc_buffer, ~labels].do {
            arg data;
            data.clear;
        };
        "Points reset.".postln;
    };
    ~set_button.(bounds, states, reset);
};

~set_show_all_points_button = {
    arg bounds;
    var states = [["Show all points"]];
    var action = {
        ~timbre_data.print;
    };
    ~set_button.(bounds, states, action);
};

~set_train_button = {
    arg bounds;
    var states = [["Train"]];
    var train = {
        var post_loss = {
            arg loss;
            loss.postln;
        };
        ~neural_network.fit(~timbre_data, ~labels, post_loss);
    };
    ~neural_network = ~get_neural_network.();
    ~set_button.(bounds, states, train);
};

~set_predict_point_button = {
    arg bounds;
    var states = [["Predict"]];
    var predict_point = {
        var post_label = {
            arg label;
            label.postln;
        };
        ~neural_network.predictPoint(~mfcc_buffer, post_label);
    };
    ~set_button.(bounds, states, predict_point);
};

~play_prediction_synth = {
    arg predictions, instrument_name;
    ~waveform = ~display_waveform.();
    ~instrument_name = instrument_name;
    ~synth = {
        ~get_synth.(make_predictions: true, predictions: predictions);
    }.play;
};

~set_prediction_action = {
    var post_message = {
        arg message;
        var prediction = message[3];
        switch (
            prediction,
            0.0, {
                "trombone".postln;
            }, 1.0, {
                "oboe".postln;
            }, {
                "silence".postln;
            };
        );
    };
    OSCdef(\predictions, post_message, "/prediction");
};

~make_predictions = {
    arg instrument_name;
    var predictions = Buffer.alloc(s, numFrames: 1);
    ~set_prediction_action.();
    ~play_prediction_synth.(predictions, instrument_name);
};

~set_predictions_button = {
    arg bounds, audio_buffer, instrument_name;
    var states = [["Predict %".format(instrument_name)]];
    var play_and_stop_predictions = {
        if (~synth == nil) {
            "Playing predictions...".postln;
            ~audio_buffer = audio_buffer;
            ~make_predictions.(instrument_name);
        };
    };
    ~set_button.(bounds, states, play_and_stop_predictions);
};

~load_audio_file = {
    arg filename, start_frame = 0, num_frames = -1;
    var path = FluidFilesPath(filename);
    Buffer.read(s, path, start_frame, num_frames);
};

~get_training_audio = {
    arg filename;
    ~load_audio_file.(filename, num_frames: ~split_frame);
};

~get_testing_audio = {
    arg filename;
    ~load_audio_file.(filename, start_frame: ~split_frame);
};

~get_window = {
    var trombone_testing_audio = ~get_testing_audio.(~trombone_file);
    var oboe_testing_audio = ~get_testing_audio.(~oboe_file);
    ~window = Window("MFCC Classifier").background_(~background).front;
    ~set_play_button.(
        Rect(10, 10, 200, 50),
        ~get_training_audio.(~trombone_file),
        "trombone"
    );
    ~set_play_button.(
        Rect(10, 60, 200, 50),
        ~get_training_audio.(~oboe_file),
        "oboe"
    );
    ~set_play_button.(
        Rect(10, 110, 200, 50),
        trombone_testing_audio,
        "trombone",
        testing: true
    );
    ~set_play_button.(
        Rect(10, 160, 200, 50),
        oboe_testing_audio,
        "oboe",
        testing: true
    );
    ~set_stop_playing_button.(Rect(10, 210, 200, 50));
    ~set_add_points_button.(Rect(220, 10, 200, 50));
    ~set_reset_button.(Rect(220, 60, 200, 50));
    ~set_train_button.(Rect(220, 110, 200, 50));
    ~set_show_all_points_button.(Rect(220, 210, 200, 50));
    ~set_predict_point_button.(Rect(220, 160, 200, 50));
    ~set_predictions_button.(
        Rect(430, 10, 200, 50),
        trombone_testing_audio,
        "trombone"
    );
    ~set_predictions_button.(
        Rect(430, 60, 200, 50),
        oboe_testing_audio,
        "oboe"
    );
};

s.waitForBoot {
    ~split_frame = 1783040;
    ~trombone_file = "Olencki-TenTromboneLongTones-M.wav";
    ~oboe_file = "Harker-DS-TenOboeMultiphonics-M.wav";
    "helpers.scd".load;
    ~reset.();
    ~get_window.();
};
)