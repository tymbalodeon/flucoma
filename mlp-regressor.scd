(
~get_synth_slider = {
    arg bounds;
    var set_synth_slider_values = {
        arg synth_slider;
        ~synth_buffer.setn(0, synth_slider.());
    };
    ~synth_buffer = Buffer.alloc(s, numFrames: 10);
    MultiSliderView(~window, bounds)
    .elasticMode_(1)
    .isFilled_(1)
    .background_(Color.black)
    .colors_(Color.white)
    .fillColor_(Color.white)
    .indexThumbSize_(50)
    .action_(set_synth_slider_values)
    .value_(0.5.dup(10));
};

~set_x_y_slider = {
    arg bounds;
    var set_x_y_values = {
        arg slider;
        ~x_y_buffer.setn(0, [slider.x, slider.y]);
    };
    ~x_y_buffer = Buffer.alloc(s, numFrames: 2);
    Slider2D(~window, bounds)
    .action_(set_x_y_values)
    .background_(Color.black);
};

~set_add_points_button = {
    arg bounds;
    var states = [["Add Points"]];
    var add_point = {
        var point = "point %".format(~counter);
        ~x_y_data.addPoint(point, ~x_y_buffer);
        ~synth_data.addPoint(point, ~synth_buffer);
        ~counter = ~counter + 1;
        ~x_y_data.print;
        ~synth_data.print;
    };
    ~counter = 0;
    ~x_y_data = FluidDataSet();
    ~synth_data = FluidDataSet();
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

~set_save_data_button = {
    arg bounds;
    var states = [["Save Data"]];
    var save_data = ~get_save_file_dialog;
    ~set_button.(bounds, states, save_data);
};

~get_load_file_dialog = {
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

~set_load_data_button = {
    arg bounds;
    var states = [["Load Data"]];
    var load_data = ~get_load_file_dialog;
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
    arg bounds;
    var states = [["Train"]];
    var train = ~train_regressor;
    ~set_button.(bounds, states, train);
};

~get_save_regressor_dialog = {
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

~set_save_mlp_button = {
    arg bounds;
    var states = [["Save MLP"]];
    var save_mlp = ~get_save_regressor_dialog;
    ~set_button.(bounds, states, save_mlp);
};

~get_load_regressor_dialog = {
    var read_json_data = {
        arg path;
        ~mlp_regressor.read(path);
    };
    Dialog.openPanel(read_json_data);
};

~set_load_mlp_button = {
    arg bounds;
    var states = [["Load MLP"]];
    var load_mlp = ~get_load_regressor_dialog;
    ~set_button.(bounds, states, load_mlp);
};

~set_predicting_button = {
    arg bounds;
    var states = [["Not Predicting"], ["Predicting"]];
    var set_state = {
        arg button;
        ~predicting = button.value;
        if (~synth != nil) {
            ~synth.set(\predicting, ~predicting);
        };
        if (~predicting == 0) {
            "Not predicting.".postln;
        } {
            "Predicting.".postln;
        }
    };
    ~predicting = 0;
    ~set_button.(bounds, states, set_state);
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
    }.play(args: [\predicting, ~predicting]);
};

~set_play_synth_button = {
    arg bounds;
    var states = [["Play"], ["Stop"]];
    var play_and_stop = {
        arg button;
        if (button.value == 1) {
            ~synth = ~play_synth.();
            "Playing...".postln;
        } {
            ~synth.free;
            ~synth = nil;
            "Playing stopped.".postln;
        };
    };
    ~set_button.(bounds, states, play_and_stop);
};

~set_window_items = {
    ~window = Window("MLP Regressor")
    .background_(Color.new255(105, 105, 105))
    .front;
    ~synth_slider = ~get_synth_slider.(Rect(320, 10, 400, 300));
    ~set_x_y_slider.(Rect(10, 10, 300, 300));
    ~set_add_points_button.(Rect(730, 10, 100, 20));
    ~set_save_data_button.(Rect(730, 40, 100, 20));
    ~set_load_data_button.(Rect(730, 70, 100, 20));
    ~set_train_button.(Rect(730, 100, 100, 20));
    ~set_save_mlp_button.(Rect(730, 130, 100, 20));
    ~set_load_mlp_button.(Rect(730, 160, 100, 20));
    ~set_predicting_button.(Rect(730, 190, 100, 20));
    ~set_play_synth_button.(Rect(730, 220, 100, 20));
    ~set_params_changed_action.();
};

~get_mlp_regressor = {
    var activation = FluidMLPRegressor.sigmoid;
    FluidMLPRegressor(
        hiddenLayers: [7],
        activation: activation,
        outputActivation: activation,
        maxIter: 1000,
        learnRate: 0.1,
        batchSize: 1,
        validation: 0
    );
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

s.waitForBoot {
    "helpers.scd".load;
    ~reset.();
    ~mlp_regressor = ~get_mlp_regressor.();
    ~set_window_items.();
};
)
