(
~load_audio_files = {
    arg file_names;
    var start_frame = 27402;
    var num_frames = 257199;
    ~audio_files = Array();
    file_names.do {
        arg file_name;
        var audio_file = ~load_audio_file.(
            file_name,
            start_frame,
            num_frames
        );
        ~audio_files.add(audio_file);
    };
};

s.waitForBoot {
    ~reset.();
    "audio_file.scd".load;
    "gui.scd".load;
    "post.scd".load;
    ~start_frame = 27402;
    ~num_frames = 257199;
    ~trombone = ~load_audio_file.(
        "Olencki-TenTromboneLongTones-M.wav",
        ~start_frame,
        ~num_frames
    );
    ~oboe = ~load_audio_file.(
        "Harker-DS-TenOboeMultiphonics-M.wav",
        ~start_frame,
        ~num_frames
    );
    // ~window = Window("MLP Training").front;
    ~data = FluidDataSet();
    ~labels = FluidLabelSet();
    ~pitch_features = Buffer();
    ~point = Buffer();
    ~is_done = CondVar();
    fork {
        var condition = CondVar();
        var audio_files = [~trombone, ~oboe];
        var last_index = audio_files.size - 1;
        audio_files.do {
            arg input, instrument_index;
            var number_of_features;
            FluidBufPitch.processBlocking(
                s,
                input,
                features: ~pitch_features,
                windowSize: 2048,
                action: {
                    arg features;
                    number_of_features = features.numFrames;
                    condition.signalOne;
                }
            );
            condition.wait {
                number_of_features != nil;
            };
            number_of_features.do {
                arg frame_index;
                var id = "Point %".format((instrument_index * number_of_features) + frame_index);
                var label = ["trombone", "oboe"][instrument_index];
                FluidBufFlatten.processBlocking(
                    s,
                    ~pitch_features,
                    frame_index,
                    1,
                    destination:~point
                );
                ~data.addPoint(id, ~point);
                ~labels.addLabel(id, label);
            };
            if (instrument_index == last_index) {
                ~is_done.signalAll;
            };
        };
    };
    fork {
        ~is_done.wait;
        FluidNormalize().fitTransform(~data, ~data);
        ~data.size.postln;
        ~data.print;
        ~labels.print;
        ~data.dump {
            arg dictionary;
            dictionary.postln;
            ~labels.dump {
                arg labels;
                defer{
                    FluidPlotter(dict: dictionary).categories_(labels);
                };
            };
        };
    };
    // ~mlp_regressor = FluidMLPClassifier(
    //     s,
    //     [3],
    //     activation: FluidMLPClassifier.sigmoid,
    //     maxIter: 20,
    //     learnRate: 0.01,
    //     batchSize: 1,
    //     validation: 0.1
    // );
    // ~train = {
    //     ~mlp_regressor.fit(
    //         ~data,
    //         ~labels,
    //         {
    //             arg loss;
    //             ~post_loss.(loss);
    //             if (~continuous_training) {
    //                 ~train.()
    //             };
    //         }
    //     );
    // };
    // ~train.();

    // you can make adjustments while it's recursive calling itself:
    // ~mlp_regressor.learnRate_(0.02);  // won't reset the neural network
    // ~mlp_regressor.batchSize_(2);     // won't reset the neural network
    // ~mlp_regressor.maxIter_(50);      // won't reset the neural network
    // ~mlp_regressor.validation_(0.05); // won't reset the neural network
    // ~mlp_regressor.momentum_(0.95);   // won't reset the neural network

    // ~mlp_regressor.hiddenLayers_([2]);      // *will* reset the neural network
    // ~mlp_regressor.activation_(FluidMLPClassifier.tanh); // *will* reset the neural network

    // // when the loss has decreased and then leveled out, stop the recursive training:
    // ~continuous_training = false;
};
)