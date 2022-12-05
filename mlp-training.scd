(
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


s.waitForBoot {
    "audio_file.scd".load;
    "gui.scd".load;
    "post.scd".load;
    ~reset.();
    ~window = Window("MLP Training").front;
    ~data = FluidDataSet();
    ~labels = FluidLabelSet();
    ~pitch_features = Buffer();
    ~point = Buffer();
    [~trombone, ~oboe].do {
        arg input, instrument_index;
        var number_of_features;
        FluidBufPitch.processBlocking(
            s,
            input,
            features: ~pitch_features,
            windowSize: 2048
            action: {
                arg features;
                number_of_features = features.numFrames;
            }
        );
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
    };
    FluidNormalize().fitTransform(~data, ~data);
    ~data.print;
    ~labels.print;
    ~data.dump {
        arg data;
        ~labels.dump {
            arg labels;
            defer{
                FluidPlotter(dict: data).categories_(labels);
            };
        };
    };
    ~mlp_regressor = FluidMLPClassifier(
        s,
        [3],
        activation: FluidMLPClassifier.sigmoid,
        maxIter: 20,
        learnRate: 0.01,
        batchSize: 1,
        validation: 0.1
    );
    ~train = {
        ~mlp_regressor.fit(
            ~data,
            ~labels,
            {
                arg loss;
                ~post_loss.(loss);
                if (~continuous_training) {
                    ~train.()
                };
            }
        );
    };
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