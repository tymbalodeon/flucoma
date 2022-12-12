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

~set_reset_button = {
    arg bounds;
    var states = [["Reset", ~background, ~yellow]];
    var action = {
        arg button;
        ~get_mlp_classifier.();
        "Reset MLP Classifier to default.".postln;
    };
    ~set_button.(bounds, states, action);
};

~set_plot_points_button = {
    arg bounds;
    var states = [["Plot Points", ~foreground, ~gray]];
    var action = {
        arg button;
        ~plot_points.();
    };
    ~set_button.(bounds, states, action);
};

~set_ez_slider = {
    arg bounds, label, action, control_spec = ControlSpec();
    EZSlider(
        parent: ~window,
        bounds: bounds,
        label: label,
        controlSpec: control_spec,
        action: action,
        layout: \vert
    ).setColors(
        stringBackground: ~text_background,
        stringColor: ~text,
        sliderBackground: ~highlight_background,
        numBackground: ~text_background,
        numNormalColor: ~text,
        numTypingColor: ~yellow,
        knobColor: ~text_background,
    );
};

~set_learn_rate_button = {
    arg bounds;
    var action = {
        arg slider;
        ~mlp_classifier.learnRate_(slider.value);
    };
    ~set_ez_slider.(bounds, "Learn Rate", action);
};

~set_batch_size_button = {
    arg bounds;
    var action = {
        arg slider;
        ~mlp_classifier.batchSize_(slider.value);
    };
    var control_spec = ControlSpec(minval: 1, maxval: 100);
    ~set_ez_slider.(bounds, "Batch Size", action, control_spec);
};

~set_max_iter_button = {
    arg bounds;
    var action = {
        arg slider;
        ~mlp_classifier.maxIter_(slider.value);
    };
    var control_spec = ControlSpec(minval: 1, maxval: 100);
    ~set_ez_slider.(bounds, "Max Iter", action, control_spec);
};

~set_validation_button = {
    arg bounds;
    var action = {
        arg slider;
        ~mlp_classifier.validation_(slider.value);
    };
    var control_spec = ControlSpec(maxval: 0.9);
    ~set_ez_slider.(bounds, "Validation", action, control_spec);
};

~set_momentum_button = {
    arg bounds;
    var action = {
        arg slider;
        ~mlp_classifier.validation_(slider.value);
    };
    var control_spec = ControlSpec(maxval: 0.99);
    ~set_ez_slider.(bounds, "Momentum", action, control_spec);
};

~set_hidden_layers_button = {
    arg bounds;
    var action = {
        arg slider;
        ~mlp_classifier.hiddenLayers_([slider.value]);
    };
    var control_spec = ControlSpec(minval: 1, maxval: 50, step: 1);
    ~set_ez_slider.(bounds, "Hidden Layers", action, control_spec);
};

~set_activation_button = {
    arg bounds;
    var action = {
        arg slider;
        ~mlp_classifier.activation_(FluidMLPClassifier.tanh);
    };
    var control_spec = ControlSpec(minval: 1, maxval: 4, step: 1);
    EZPopUpMenu(
        parentView: ~window,
        bounds: bounds,
        label: "Activation",
        items: [
            \identity -> FluidMLPClassifier.identity,
            \sigmoid -> FluidMLPClassifier.sigmoid,
            \relu -> FluidMLPClassifier.relu,
            \tanh -> FluidMLPClassifier.tanh
        ],
        layout: \vert
    ).setColors(
        stringBackground: ~text_background,
        stringColor: ~text,
    );
};

    ~set_window_items = {
        ~window = ~get_window.("MLP Training");
        ~set_train_button.(Rect(10, 10, 150, 30));
        ~set_reset_button.(Rect(170, 10, 150, 30));
        ~set_plot_points_button.(Rect(330, 10, 150, 30));
        ~set_learn_rate_button.(Rect(10, 50, 70, 200));
        ~set_batch_size_button.(Rect(90, 50, 70, 200));
        ~set_max_iter_button.(Rect(170, 50, 70, 200));
        ~set_validation_button.(Rect(250, 50, 70, 200));
        ~set_momentum_button.(Rect(330, 50, 70, 200));
        ~set_hidden_layers_button.(Rect(10, 260, 90, 200));
        ~set_activation_button.(Rect(110, 260, 90, 50));
    };

    s.waitForBoot {
        "audio_file.scd".load;
        "gui.scd".load;
        "post.scd".load;
        "themes.scd".load;
        ~reset.();
        ~load_theme.(\gruvbox);
        ~text_background = ~background;
        ~text = ~foreground;
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
        ~set_window_items.();
    };
)