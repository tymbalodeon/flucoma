(
~get_points = {
    fork {
        var condition = CondVar();
        var last_index = ~audio_files.size - 1;
        ~audio_files.do {
            arg input, instrument_index;
            var number_of_features;
            FluidBufPitch.processBlocking(
                source: input,
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
                var start_index = instrument_index * number_of_features;
                var point_number = start_index + frame_index;
                var id = "Point %".format(point_number);
                var label = ["trombone", "oboe"][instrument_index];
                FluidBufFlatten.processBlocking(
                    source: ~pitch_features,
                    startFrame: frame_index,
                    numFrames: 1,
                    destination: ~point
                );
                ~data.addPoint(id, ~point);
                ~labels.addLabel(id, label);
            };
            if (instrument_index == last_index) {
                ~is_done.signalAll;
            };
        };
    };
};

~plot_points = {
    fork {
        ~is_done.wait;
        FluidNormalize().fitTransform(~data, ~data);
        ~data.dump {
            arg dictionary;
            ~labels.dump {
                arg labels;
                defer{
                    FluidPlotter(dict: dictionary).categories_(labels);
                };
            };
        };
    };
};

~get_mlp_regressor = {
    ~mlp_regressor = FluidMLPClassifier(
        hiddenLayers: [3],
        activation: FluidMLPClassifier.sigmoid,
        maxIter: 20,
        learnRate: 0.01,
        batchSize: 1,
        validation: 0.1
    );
};

~train = {
    ~mlp_regressor.fit(
        ~data,
        ~labels,
        {
            arg loss;
            ~post_loss.(loss);
            if (~is_training) {
                ~train.()
            };
        }
    );
};

~set_train_button = {
    arg bounds;
    var states = [["Train"], ["Stop"]];
    var action = {
        arg button;
        if (button.value == 0) {
            ~is_training = false;
        } {
            ~is_training = true;
            ~train.();
        };
    };
    ~get_mlp_regressor.();
    ~is_training = true;
    ~set_button.(bounds, states, action);
};

~set_learn_rate_button = {

};

~set_batch_size_button = {

};

~get_window = {
    ~window = Window("MLP Training").front;
    ~set_train_button.(Rect(10, 10, 150, 30));
};

s.waitForBoot {
    "audio_file.scd".load;
    "gui.scd".load;
    "post.scd".load;
    ~reset.();
    ~file_names = [
        "Olencki-TenTromboneLongTones-M.wav",
        "Harker-DS-TenOboeMultiphonics-M.wav"
    ];
    ~audio_files = ~get_audio_files.(
        ~file_names,
        start_frame: 27402,
        num_frames: 257199
    );
    ~data = FluidDataSet();
    ~labels = FluidLabelSet();
    ~pitch_features = Buffer();
    ~point = Buffer();
    ~is_done = CondVar();
    ~get_points.();
    // ~plot_points.();
    ~get_window.();

    // ~mlp_regressor.learnRate_(0.02);  // won't reset the neural network
    // ~mlp_regressor.batchSize_(2);     // won't reset the neural network
    // ~mlp_regressor.maxIter_(50);      // won't reset the neural network
    // ~mlp_regressor.validation_(0.05); // won't reset the neural network
    // ~mlp_regressor.momentum_(0.95);   // won't reset the neural network

    // ~mlp_regressor.hiddenLayers_([2]);      // *will* reset the neural network
    // ~mlp_regressor.activation_(FluidMLPClassifier.tanh); // *will* reset the neural network
};
)