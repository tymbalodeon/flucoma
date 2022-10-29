(
~close_all_windows = {
	Window.closeAll
};

~get_window = {
	var name = "Chaos Synth";
	var bounds = Rect(10, 10, 840, 320);
	Window(name, bounds).front
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
	)
};

~get_parameters = {
	var num_frames = 10;
	Buffer.alloc(s, num_frames)
};

~get_multi_slider = {
	arg parent, parameters;
	var bounds = Rect(10, 10, 400, 300);
	var action = {
		arg multi_slider;
		parameters.setn(0, multi_slider.value);
	};
	MultiSliderView(parent, bounds)
	.elasticMode_(1)
	.isFilled_(1)
	.action_(action)
	.value_(0.5.dup(10))
};

~get_x_y_buffer = {
	var num_frames = 2;
	Buffer.alloc(s, num_frames)
};

~get_x_y_slider = {
	arg parent, x_y_buffer;
	var bounds = Rect(420, 10, 300, 300);
	var action = {
		arg view;
		x_y_buffer.setn(0, [view.x, view.y]);
	};
	Slider2D(parent, bounds).action_(action)
};

~get_state = {
	Dictionary.newFrom([\counter, 0])
};

~set_button = {
	arg parent, bounds, states, action;
	Button(parent, bounds).states_(states).action_(action)
};

~set_add_points_button = {
	arg parent, state, x_y_data, x_y_buffer, parameter_data, parameters;
	var bounds = Rect(730, 10, 100, 20);
	var states = [["Add Points"]];
	var action = {
		var id = "point-%".format(state[\counter]);
		x_y_data.addPoint(id, x_y_buffer);
		parameter_data.addPoint(id, parameters);
		state[\counter] = state[\counter] + 1;
		x_y_data.print;
		parameter_data.print;
	};
	~set_button.value(parent, bounds, states, action)
};

~set_save_data_button = {
	arg parent, x_y_data, parameter_data;
	var bounds = Rect(730, 40, 100, 20);
	var states = [["Save Data"]];
	var action = {
		var ok_func = {
			arg folder;
			x_y_data.write(folder +/+ "x-y-data.json");
			parameter_data.write(folder +/+ "parameter-data.json");
		};
		FileDialog(
			ok_func,
			cancelFunc: {},
			fileMode: 2,
			acceptMode: 0,
			stripResult: true
		)
	};
	~set_button.value(parent, bounds, states, action)
};

~set_load_data_button = {
	arg parent, x_y_data, parameter_data;
	var bounds = Rect(730, 70, 100, 20);
	var states = [["Load Data"]];
	var action = {
		var ok_func = {
			arg folder;
			x_y_data.write(folder +/+ "x-y-data.json");
			parameter_data.write(folder +/+ "parameter-data.json");
		};
		FileDialog(
			ok_func,
			fileMode: 2,
			acceptMode: 0,
			stripResult: true
		);
	};
	~set_button.value(parent, bounds, states, action)
};

~set_train_button = {
	arg parent, mlp_regressor, x_y_data, parameter_data;
	var bounds = Rect(730, 100, 100, 20);
	var states = [["Train"]];
	var action = {
		var post_loss = {
			arg loss;
			loss.postln;
		};
		mlp_regressor.fit(x_y_data, parameter_data, post_loss);
	};
	~set_button.value(parent, bounds, states, action)
};

~set_save_mlp_button = {
	arg parent, mlp_regressor;
	var bounds = Rect(730, 130, 100, 20);
	var states = [["Save MLP"]];
	var action = {
		var ok_func = {
			arg path;
			var extension = PathName(path).extension;
			if (extension != "json") {
				path = "%.json".format(path);
			};
			mlp_regressor.write(path);
		};
		Dialog.savePanel(ok_func)
	};
	~set_button.value(parent, bounds, states, action)
};

~set_load_mlp_button = {
	arg parent, mlp_regressor;
	var bounds = Rect(730, 160, 100, 20);
	var states = [["Load MLP"]];
	var action = {
		var ok_func = {
			arg path;
			mlp_regressor.read(path);
		};
		Dialog.openPanel(ok_func);
	};
	~set_button.value(parent, bounds, states, action)
};

~set_predicting_button = {
	arg parent, synth;
	var bounds = Rect(730, 190, 100, 20);
	var states = [["Not Predicting"], ["Predicting"]];
	var action = {
		arg button;
		synth.set(\predicting, button.value);
	};
	~set_button.value(parent, bounds, states, action)
};

~set_buttons = {
	arg window, state, x_y_data, x_y_buffer,
	    parameter_data, mlp_regressor, parameters, synth;
	~set_add_points_button.value(
		window,
		state,
		x_y_data,
		x_y_buffer,
		parameter_data,
		parameters
	);
	~set_save_data_button.value(window, x_y_data, parameter_data);
	~set_load_data_button.value(window, x_y_data, parameter_data);
	~set_train_button.value(window, mlp_regressor, x_y_data, parameter_data);
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
	SinOsc.ar(freq, mul: mul).atan
};

~get_moog_ff = {
	arg feed, base, value, loudness, index_1, index_2, index_3, index_4, index_5;
	var osc = ~get_sin_osc.value(feed, base, value, index_1, index_2, index_3);
	var base_3 = 130;
	MoogFF.ar(
		osc,
		(base_3 - (value[index_4] * (loudness.clip(-120, 0) + 120)))
			.lag(128 / 44100)
			.midicps,
		value[index_5] * 3.5)
};

~synth = {
	arg x_y_buffer, mlp_regressor, parameters, predicting = 0;
	var x_y = FluidBufToKr.kr(x_y_buffer);
	var trigger = Mix(Changed.kr(x_y));
	var value = FluidBufToKr.kr(parameters);
	var feed_1 = LocalIn.ar(2)[0];
	var feed_2 = LocalIn.ar(2)[1];
	var loudness_1 = ~get_loudness.value(feed_1);
	var loudness_2 = ~get_loudness.value(feed_2);
	var base_1 = 69;
	var base_2 = 69;
	var osc_1 = ~get_moog_ff.value(feed_1, base_1, value, loudness_1, 0, 1, 2, 3, 4);
	var osc_2 = ~get_moog_ff.value(feed_2, base_2, value, loudness_2, 5, 6, 7, 8, 9);
	var signals = [osc_1, osc_2];
	mlp_regressor.kr(trigger * predicting, x_y_buffer, parameters);
	SendReply.kr(
		trig: Mix(Changed.kr(value)),
		cmdName: "/paramsChanged",
		values: value
	);
	Out.ar(0, LeakDC.ar(signals, mul: 0.1));
	LocalOut.ar(signals);
};

~play_synth = {
    arg synth;
    {synth}.play;
};

~send_params_changed_message = {
	arg multi_slider;
	var func = {
		arg message;
		defer {
			multi_slider.value_(message[3..]);
		};
	};
	OSCdef(\paramsChanged, func, "/paramsChanged");
};

~start_mlp_regressor = {
	var window = ~get_window.value;
	var parameters = ~get_parameters.value;
	var parameter_data = FluidDataSet(s);
	var mlp_regressor = ~get_mlp_regressor.value;
	var multi_slider = ~get_multi_slider.value(window, parameters);
	var x_y_buffer = ~get_x_y_buffer.value;
	var x_y_slider = ~get_x_y_slider.value(window, x_y_buffer);
	var state = ~get_state.value;
	var x_y_data = FluidDataSet(s);
	var synth = ~synth.value(x_y_buffer, mlp_regressor, parameters);
	~set_buttons.value(
		window,
		state,
		x_y_data,
		x_y_buffer,
		parameter_data,
		mlp_regressor,
		parameters,
		synth
	);
	s.sync;
	~play_synth.value(synth);
	~send_params_changed_message.value(multi_slider);
};

s.waitForBoot {
	~close_all_windows.value;
    ServerMeter(s, 2, 2);
	~start_mlp_regressor.value
}
)