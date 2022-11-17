(
~get_mlp_regressor = {
    var activation = FluidMLPRegressor.sigmoid;
    FluidMLPRegressor(
        s,
        hiddenLayers: [7],
        activation: activation,
        outputActivation: activation,
        maxIter: 1000,
        learnRate: 0.1,
        batchSize: 1,
        validation: 0
    );
};

~get_synth_slider = {
    var bounds = Rect(10, 10, 400, 300);
    var set_synth_slider_values = {
        arg synth_slider;
        ~synth_buffer.setn(0, synth_slider.());
    };
    MultiSliderView(~window, bounds)
    .elasticMode_(1)
    .isFilled_(1)
    .action_(set_synth_slider_values)
    .value_(0.5.dup(10));
};

~set_button = {
    arg bounds, states, action;
    Button(~window, bounds).states_(states).action_(action);
};

~set_add_points_button = {
    var bounds = Rect(730, 10, 100, 20);
    var states = [["Add Points"]];
    var add_point = {
        var point = "point %".format(~counter);
        ~x_y_data.addPoint(point, ~x_y_buffer);
        ~synth_data.addPoint(point, ~synth_buffer);
        ~counter = ~counter + 1;
        ~x_y_data.print;
        ~synth_data.print;
    };
    ~set_button.(bounds, states, add_point);
};

~get_x_y_data_json_file = {
    arg folder;
    folder +/+ "x-y-data.json";
};

~get_synth_data_json_file = {
    arg folder;
    folder +/+ "synth-data.json";
};

~get_save_file_dialog = {
    {
        var write_json_data = {
            arg folder;
            var x_y_data_json_file = ~get_x_y_data_json_file.(folder);
            var synth_data_json_file = ~get_synth_data_json_file.(folder);
            ~x_y_data.write(x_y_data_json_file);
            ~synth_data.write(synth_data_json_file);
        };
        FileDialog(
            write_json_data,
            cancelFunc: {},
            fileMode: 2,
            acceptMode: 0,
            stripResult: true
        )
    };
};

~set_save_data_button = {
    var bounds = Rect(730, 40, 100, 20);
    var states = [["Save Data"]];
    var save_data = ~get_save_file_dialog.();
    ~set_button.(bounds, states, save_data);
};

~get_load_file_dialog = {
    {
        var write_json_data = {
            arg folder;
            var x_y_data_json_file = ~get_x_y_data_json_file.(folder);
            var synth_data_json_file = ~get_synth_data_json_file.(folder);
            ~x_y_data.read(x_y_data_json_file);
            ~synth_data.read(synth_data_json_file);
        };
        FileDialog(
            write_json_data,
            fileMode: 2,
            acceptMode: 0,
            stripResult: true
        );
    };
};

~set_load_data_button = {
    var bounds = Rect(730, 70, 100, 20);
    var states = [["Load Data"]];
    var load_data = ~get_load_file_dialog.();
    ~set_button.(bounds, states, load_data);
};

~train_regressor = {
    var post_loss = {
        arg loss;
        "Training loss: %".format(loss).postln;
    };
    ~mlp_regressor.fit(~x_y_data, ~synth_data, post_loss);
};

~set_train_button = {
    var bounds = Rect(730, 100, 100, 20);
    var states = [["Train"]];
    var train = ~train_regressor.();
    ~set_button.(bounds, states, train);
};

~get_save_regressor_dialog = {
    {
        var write_json_data = {
            arg path;
            var extension = PathName(path).extension;
            if (extension != "json") {
                path = "%.json".format(path);
            };
            ~mlp_regressor.write(path);
        };
        Dialog.savePanel(write_json_data, path: "mlp-regressor.json");
    };
};

~set_save_mlp_button = {
    var bounds = Rect(730, 130, 100, 20);
    var states = [["Save MLP"]];
    var save_mlp = ~get_save_regressor_dialog.();
    ~set_button.(bounds, states, save_mlp);
};

~get_load_regressor_dialog = {
    {
        var read_json_data = {
            arg path;
            ~mlp_regressor.read(path);
        };
        Dialog.openPanel(read_json_data);
    };
};

~set_load_mlp_button = {
    var bounds = Rect(730, 160, 100, 20);
    var states = [["Load MLP"]];
    var load_mlp = ~get_load_regressor_dialog.();
    ~set_button.(bounds, states, load_mlp);
};

~set_predicting_button = {
    var bounds = Rect(730, 190, 100, 20);
    var states = [["Not Predicting"], ["Predicting"]];
    var set_state = {
        arg button;
        ~synth.set(\predicting, button.());
    };
    ~set_button.(bounds, states, set_state);
};

~set_window_items = {
    ~set_x_y_slider.();
    ~set_add_points_button.();
    ~set_save_data_button.();
    ~set_load_data_button.();
    ~set_train_button.();
    ~set_save_mlp_button.();
    ~set_load_mlp_button.();
    ~set_predicting_button.();
};

~get_loudness = {
    arg feed;
    FluidLoudness.kr(
        feed,
        select: [\loudness],
        kWeighting: 1,
        truePeak: 0,
        hopSize: 64
    );
};

~get_sin_osc = {
    arg feed, base, value, index_1, index_2, index_3;
    var freq = (((feed * value[index_1]) +  value[index_2]) * base).midicps;
    var mul = (value[index_3] * 50).dbamp;
    SinOsc.ar(freq, mul: mul).atan;
};

~get_moog_ff = {
    arg feed, base, value, loudness,
    index_1, index_2, index_3, index_4, index_5;
    var osc = ~get_sin_osc.(feed, base, value, index_1, index_2, index_3);
    var base_3 = 130;
    MoogFF.ar(
        osc,
        (base_3 - (value[index_4] * (loudness.clip(-120, 0) + 120)))
        .lag(128 / 44100)
        .midicps,
        value[index_5] * 3.5);
};

~play_synth = {
    {
        arg predicting = 0;
        var x_y = FluidBufToKr.kr(~x_y_buffer);
        var trigger = Mix(Changed.kr(x_y));
        var value = FluidBufToKr.kr(~synth_buffer);
        var local_in = LocalIn.ar(2);
        var feed_1 = local_in[0];
        var feed_2 = local_in[1];
        var loudness_1 = ~get_loudness.(feed_1);
        var loudness_2 = ~get_loudness.(feed_2);
        var base = 69;
        var osc_1 = ~get_moog_ff.(
            feed_1,
            base,
            value,
            loudness_1,
            0, 1, 2, 3, 4
        );
        var osc_2 = ~get_moog_ff.(
            feed_2,
            base,
            value,
            loudness_2,
            5, 6, 7, 8, 9
        );
        var signals = [osc_1, osc_2];
        ~mlp_regressor.kr(trigger * predicting, ~x_y_buffer, ~synth_buffer);
        SendReply.kr(
            trig: Mix(Changed.kr(value)),
            cmdName: "/params_changed",
            values: value
        );
        Out.ar(0, LeakDC.ar(signals, mul: 0.1));
        LocalOut.ar(signals);
    }.play;
};

~set_params_changed_action = {
    var set_synth_slider_value = {
        arg message;
        defer {
            ~synth_slider.value_(message[3..]);
        };
    };
    OSCdef(\params_changed, set_synth_slider_value, "/params_changed");
};

~set_x_y_slider = {
    var bounds = Rect(420, 10, 300, 300);
    var set_x_y_values = {
        arg slider;
        ~x_y_buffer.setn(0, [slider.x, slider.y]);
    };
    Slider2D(~window, bounds).action_(set_x_y_values);
};

~reset_windows = {
    Window.closeAll;
    ServerMeter(s, 2, 2);
};

~set_state = {
    ~counter = 0;
    ~window = Window("MLP Regressor").front;
    ~synth_buffer = Buffer.alloc(s, numFrames: 10);
    ~synth_slider = ~get_synth_slider.value();
    ~x_y_buffer = Buffer.alloc(s, numFrames: 2);
    ~synth_data = FluidDataSet(s);
    ~x_y_data = FluidDataSet(s);
    ~mlp_regressor = ~get_mlp_regressor.();
    ~set_window_items.();
    ~set_params_changed_action.();
};

s.waitForBoot {
    ~reset_windows.();
    ~set_state.();
    ~synth = ~play_synth.();
};
)