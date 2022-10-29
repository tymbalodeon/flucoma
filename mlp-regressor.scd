(
~reset_windows = {
	Window.closeAll;
    ServerMeter(s, 2, 2);
};

~get_window = {
	var name = "Chaos Synth";
	var bounds = Rect(10, 10, 840, 320);
	Window(name, bounds).front;
};

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

~get_synth_buffer = {
	var num_frames = 10;
	Buffer.alloc(s, num_frames);
};

~get_synth_slider = {
	arg parent, synth_buffer;
	var bounds = Rect(10, 10, 400, 300);
	var set_synth_slider_values = {
		arg synth_slider;
		synth_buffer.setn(0, synth_slider.value);
	};
	MultiSliderView(parent, bounds)
		.elasticMode_(1)
		.isFilled_(1)
		.action_(set_synth_slider_values)
		.value_(0.5.dup(10));
};

~get_x_y_buffer = {
	var num_frames = 2;
	Buffer.alloc(s, num_frames);
};

~get_x_y_slider = {
	arg parent, x_y_buffer;
	var bounds = Rect(420, 10, 300, 300);
	var set_x_y_values = {
		arg slider;
		x_y_buffer.setn(0, [slider.x, slider.y]);
	};
	Slider2D(parent, bounds).action_(set_x_y_values);
};

~get_state = {
	Dictionary.newFrom([\counter, 0]);
};

~set_button = {
	arg parent, bounds, states, action;
	Button(parent, bounds).states_(states).action_(action);
};

~set_add_points_button = {
	arg parent, state, x_y_data,
		x_y_buffer, synth_data, synth_buffer;
	var bounds = Rect(730, 10, 100, 20);
	var states = [["Add Points"]];
	var add_point = {
		var point = "point %".format(state[\counter]);
		x_y_data.addPoint(point, x_y_buffer);
		synth_data.addPoint(point, synth_buffer);
		state[\counter] = state[\counter] + 1;
		x_y_data.print;
		synth_data.print;
	};
	~set_button.value(parent, bounds, states, add_point);
};

~set_save_data_button = {
	arg parent, x_y_data, synth_data;
	var bounds = Rect(730, 40, 100, 20);
	var states = [["Save Data"]];
	var save_data = {
		var write_json_data = {
			arg folder;
			x_y_data.write(folder +/+ "x-y-data.json");
			synth_data.write(folder +/+ "synth-data.json");
		};
		FileDialog(
			write_json_data,
			cancelFunc: {},
			fileMode: 2,
			acceptMode: 0,
			stripResult: true
		)
	};
	~set_button.value(parent, bounds, states, save_data);
};

~set_load_data_button = {
	arg parent, x_y_data, synth_data;
	var bounds = Rect(730, 70, 100, 20);
	var states = [["Load Data"]];
	var load_data = {
		var write_json_data = {
			arg folder;
			x_y_data.read(folder +/+ "x-y-data.json");
			synth_data.read(folder +/+ "synth-data.json");
		};
		FileDialog(
			write_json_data,
			fileMode: 2,
			acceptMode: 0,
			stripResult: true
		);
	};
	~set_button.value(parent, bounds, states, load_data);
};

~set_train_button = {
	arg parent, mlp_regressor, x_y_data, synth_data;
	var bounds = Rect(730, 100, 100, 20);
	var states = [["Train"]];
	var train = {
		var post_loss = {
			arg loss;
			"Training loss: %".format(loss).postln;
		};
		mlp_regressor.fit(x_y_data, synth_data, post_loss);
	};
	~set_button.value(parent, bounds, states, train);
};

~set_save_mlp_button = {
	arg parent, mlp_regressor;
	var bounds = Rect(730, 130, 100, 20);
	var states = [["Save MLP"]];
	var save_mlp = {
		var write_json_data = {
			arg path;
			var extension = PathName(path).extension;
			if (extension != "json") {
				path = "%.json".format(path);
			};
			mlp_regressor.write(path);
		};
        Dialog.savePanel(write_json_data, path: "mlp-regressor.json");
	};
	~set_button.value(parent, bounds, states, save_mlp);
};

~set_load_mlp_button = {
	arg parent, mlp_regressor;
	var bounds = Rect(730, 160, 100, 20);
	var states = [["Load MLP"]];
	var load_mlp = {
		var read_json_data = {
			arg path;
			mlp_regressor.read(path);
		};
		Dialog.openPanel(read_json_data);
	};
	~set_button.value(parent, bounds, states, load_mlp);
};

~set_predicting_button = {
	arg parent, synth;
	var bounds = Rect(730, 190, 100, 20);
	var states = [["Not Predicting"], ["Predicting"]];
	var set_state = {
		arg button;
		synth.set(\predicting, button.value);
	};
	~set_button.value(parent, bounds, states, set_state);
};

~set_buttons = {
	arg window, state, x_y_data, x_y_buffer,
	    synth_data, mlp_regressor, synth_buffer, synth;
	~set_add_points_button.value(
		window,
		state,
		x_y_data,
		x_y_buffer,
		synth_data,
		synth_buffer
	);
	~set_save_data_button.value(window, x_y_data, synth_data);
	~set_load_data_button.value(window, x_y_data, synth_data);
	~set_train_button.value(window, mlp_regressor, x_y_data, synth_data);
	~set_save_mlp_button.value(window, mlp_regressor);
	~set_load_mlp_button.value(window, mlp_regressor);
	~set_predicting_button.value(window, synth);
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
	var osc = ~get_sin_osc.value(feed, base, value, index_1, index_2, index_3);
	var base_3 = 130;
	MoogFF.ar(
		osc,
		(base_3 - (value[index_4] * (loudness.clip(-120, 0) + 120)))
			.lag(128 / 44100)
			.midicps,
		value[index_5] * 3.5);
};

~play_synth = {
    arg x_y_buffer, mlp_regressor, synth_buffer;
    {
        arg predicting = 0;
		var x_y = FluidBufToKr.kr(x_y_buffer);
		var trigger = Mix(Changed.kr(x_y));
		var value = FluidBufToKr.kr(synth_buffer);
        var local_in = LocalIn.ar(2);
		var feed_1 = local_in[0];
		var feed_2 = local_in[1];
		var loudness_1 = ~get_loudness.value(feed_1);
		var loudness_2 = ~get_loudness.value(feed_2);
		var base = 69;
		var osc_1 = ~get_moog_ff.value(feed_1, base, value, loudness_1, 0, 1, 2, 3, 4);
		var osc_2 = ~get_moog_ff.value(feed_2, base, value, loudness_2, 5, 6, 7, 8, 9);
		var signals = [osc_1, osc_2];
		mlp_regressor.kr(trigger * predicting, x_y_buffer, synth_buffer);
		SendReply.kr(
			trig: Mix(Changed.kr(value)),
			cmdName: "/params_changed",
			values: value
		);
		Out.ar(0, LeakDC.ar(signals, mul: 0.1));
		LocalOut.ar(signals);
    }.play;
};

~send_params_changed_message = {
	arg synth_slider;
	var func = {
		arg message;
		defer {
			synth_slider.value_(message[3..]);
		};
	};
	OSCdef(\params_changed, func, "/params_changed");
};

~start_mlp_regressor = {
	var window = ~get_window.value;
	var synth_buffer = ~get_synth_buffer.value;
	var synth_slider = ~get_synth_slider.value(window, synth_buffer);
	var x_y_buffer = ~get_x_y_buffer.value;
	var x_y_slider = ~get_x_y_slider.value(window, x_y_buffer);
	var synth_data = FluidDataSet(s);
	var x_y_data = FluidDataSet(s);
	var mlp_regressor = ~get_mlp_regressor.value;
	var synth = ~play_synth.value(x_y_buffer, mlp_regressor, synth_buffer);
	var state = ~get_state.value;
    ~set_buttons.value(
		window,
		state,
		x_y_data,
		x_y_buffer,
		synth_data,
		mlp_regressor,
		synth_buffer,
		synth
	);
	~send_params_changed_message.value(synth_slider);
};

s.waitForBoot {
	~reset_windows.value;
	~start_mlp_regressor.value;
}
)