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
        };
    };
};

~plot_points = {
    fork {
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

~get_mlp_classifier = {
    ~mlp_classifier = FluidMLPClassifier(
        hiddenLayers: [3],
        activation: FluidMLPClassifier.sigmoid,
        maxIter: 20,
        learnRate: 0.01,
        batchSize: 1,
        validation: 0.1
    );
};

~train = {
    ~mlp_classifier.fit(
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
    ~get_mlp_classifier.();
    ~is_training = true;
    ~set_button.(bounds, states, action);
};

~set_plot_points_button = {
    arg bounds;
    var states = [["Plot Points"]];
    var action = {
        arg button;
        ~plot_points.();
    };
    ~set_button.(bounds, states, action);
};

~set_learn_rate_button = {
    arg bounds;
    EZSlider(
        parent: ~window,
        bounds: bounds,
        label: "Learn Rate",
        action: {
            arg slider;
            ~mlp_classifier.learnRate_(slider.value);
        },
        layout: \vert
    );
};

~set_batch_size_button = {
    arg bounds;
    EZSlider(
        parent: ~window,
        bounds: bounds,
        label: "Batch Size",
        controlSpec: ControlSpec(minval: 1, maxval: 50),
        action: {
            arg slider;
            ~mlp_classifier.batchSize_(slider.value);
        },
        layout: \vert
    );
};

~set_max_iter_button = {
    arg bounds;
    EZSlider(
        parent: ~window,
        bounds: bounds,
        label: "Max Iter",
        controlSpec: ControlSpec(minval: 1, maxval: 100),
        action: {
            arg slider;
            ~mlp_classifier.maxIter_(slider.value);
        },
        layout: \vert
    );
};


~set_validation_button = {
    arg bounds;
    EZSlider(
        parent: ~window,
        bounds: bounds,
        label: "Validation",
        controlSpec: ControlSpec(maxval: 0.9),
        action: {
            arg slider;
            ~mlp_classifier.validation_(slider.value);
        },
        layout: \vert
    );
};

~set_momentum_button = {
    arg bounds;
    EZSlider(
        parent: ~window,
        bounds: bounds,
        label: "Momentum",
        controlSpec: ControlSpec(maxval: 0.99),
        action: {
            arg slider;
            ~mlp_classifier.validation_(slider.value);
        },
        layout: \vert
    );
};

~get_window = {
    ~window = Window("MLP Training").front;
    ~set_train_button.(Rect(10, 10, 150, 30));
    ~set_plot_points_button.(Rect(170, 10, 150, 30));
    ~set_learn_rate_button.(Rect(10, 50, 70, 200));
    ~set_batch_size_button.(Rect(90, 50, 70, 200));
    ~set_max_iter_button.(Rect(170, 50, 70, 200));
    ~set_validation_button.(Rect(250, 50, 70, 200));
    ~set_momentum_button.(Rect(330, 50, 70, 200));
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
    ~get_points.();
    ~get_window.();

    // ~mlp_classifier.hiddenLayers_([2]);      // *will* reset the neural network
    // ~mlp_classifier.activation_(FluidMLPClassifier.tanh); // *will* reset the neural network
};
)