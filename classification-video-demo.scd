~split_frame = 1783040;

~load_audio_file = {
    arg filename, start_frame = 0, num_frames = -1;
    var path = FluidFilesPath(filename);
    Buffer.read(s, path, start_frame, num_frames);
};

~get_training_audio = {
    arg filename
    ~load_audio_file.value(filename, num_frames: ~split_frame);
}

~get_testing_audio = {
    arg filename
    ~load_audio_file.value(filename, start_frame: ~split_frame);
}

~trombone_file = "Olencki-TenTromboneLongTones-M.wav";
~oboe_file = "Harker-DS-TenOboeMultiphonics-M.wav";
~trombone_training_audio = ~get_training_audio.value(~trombone_file);
~trombone_testing_audio = ~get_testing_audio.value(~trombone_file);
~oboe_training_audio = ~get_training_audio.value(~oboe_file);
~oboe_testing_audio = ~get_testing_audio.value(~oboe_file);

~display_waveform = {
    arg audio;
    FluidWaveform(audio);
}

(
~num_coeffs = 13;
~mfcc_buffer = Buffer.alloc(s, ~num_coeffs);
~timbre_data = FluidDataSet(s);
~labels = FluidLabelSet(s);
~counter = 0;
~analyze_and_play = {
    arg audio_buffer;
    {
        var audio = PlayBuf.ar(
            numChannels: 1,
            bufNum: audio_buffer,
            rate: BufRateScale.ir(audio_buffer),
            doneAction: 2
        );
        var mfcc = FluidMFCC.kr(
            audio,
            startCoeff: 1,
            maxNumCoeffs: ~num_coeffs
        );
        FluidKrToBuf.kr(mfcc, ~mfcc_buffer);
        audio.dup;
    }.play;
};
~add_point = {
    arg label;
    var id = "sample %".format(~counter);
    ~timbre_data.addPoint(id, ~mfcc_buffer);
    ~labels.addLabel(id, label);
    ~counter = ~counter + 1;
};
)

~timbre_data.print;
~labels.print;
~timbre_data.clear;
~labels.clear;

~analyze_and_play.value(~trombone_training_audio);
~add_point.value("trombone");

~analyze_and_play.value(~oboe_training_audio);
~add_point.value("oboe");

~get_neural_network = {
    var activation = FluidMLPRegressor.sigmoid;
    ~neural_network = FluidMLPClassifier(
        s,
        hidden: [5],
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

~get_predictions_buffer = {
    var num_frames = 1;
    Buffer.alloc(s, num_frames);
}

~set_prediction_action = {
    var post_message = {
        arg message;
        message[3].postln;
    }
    OSCdef(\predictions, post_message, "/prediction");
}

~play_prediction_synth = {
    arg audio_buffer, mfcc_buffer, neural_network, predictions;
    {
        var audio = PlayBuf.ar(1, audio_buffer, BufRateScale.ir(audio_buffer), doneAction: 2);
        var mfcc = FluidMFCC.kr(audio, ~nmfccs, startCoeff: 1, maxNumCoeffs: ~nmfccs);
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
}

~make_predictions = {
    arg audio_buffer, mfcc_buffer, neural_network;
    var predictions = ~get_predictions_buffer.value;
    ~set_prediction_action.value;
    ~play_prediction_synth.value(audio_buffer, mfcc_buffer, neural_network, predictions);
};

~make_predictions.value(~trombone_testing_audio);
~make_predictions.value(~oboe_testing_audio);

~analyze_and_play.value(~trombone_testing_audio);
~analyze_and_play.value(~oboe_testing_audio);

~predict_point = {
    arg neural_network, mfcc_buffer;
    var post_label = {
        arg label;
        label.postln;
    }
    neural_network.predictPoint(mfcc_buffer, post_label);
}