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

(
~num_coeffs = 13;
~mfcc_buffer = Buffer.alloc(s, ~num_coeffs);
~timbre_data = FluidDataSet(s);
~labels = FluidLabelSet(s);
~counter = 0;
~analyze_and_play = {
	arg audio_buffer;
	{
		var signal = PlayBuf.ar(
			numChannels: 1,
			bufNum: audio_buffer,
			rate: BufRateScale.ir(audio_buffer),
			doneAction: 2
		);
		var mfccs = FluidMFCC.kr(
			signal,
			numCoeffs: ~num_coeffs,
			startCoeff: 1,
			maxNumCoeffs: ~num_coeffs
		);
		FluidKrToBuf.kr(mfccs, ~mfcc_buffer);
		signal.dup;
	}.play;
};
~add_point = {
	arg label;
	var id = "example %".format(~counter);
	~timbre_data.addPoint(id, ~mfcc_buffer);
	~labels.addLabel(id, label);
	~counter = ~counter + 1;
};
)

~timbre_data.print;
~labels.print;
~timbre_data.clear;
~labels.clear;

// play the playback+analysis function with the trombone sound
~analyze_and_play.value(~trombone_train);
// execute the add point function as many times as you want points
~add_point.value("trombone");

// do the same with the oboe sound
~analyze_and_play.value(~oboe_train);
// and add a similar number of points
~add_point.value("oboe");

// create a classifier
(
~neural_network = FluidMLPClassifier(
	s,
	hidden: [5],
	activation: 1,
	maxIter: 1000,
	learnRate: 0.1,
	momentum: 0.9,
	batchSize: 5,
	validation: 0
);
)

// run this fitting function for as long as the error is not acceptable
(
var post_loss = {
	arg loss;
	loss.postln;
};
~neural_network.fit(~timbre_data, ~labels, post_loss);
)

// define a querying function on the synth
(
var num_frames = 1;
~predictions = Buffer.alloc(s, num_frames);
~server_predictions = {
	arg buffer;
	var action = {
		arg message;
		message[3].postln;
	}
	OSCdef(\predictions, action, "/prediction");
	{
		var signal = PlayBuf.ar(1, buffer, BufRateScale.ir(buffer), doneAction: 2);
		var mfccs = FluidMFCC.kr(signal, ~nmfccs, startCoeff: 1, maxNumCoeffs: ~nmfccs);
		var loudness = FluidLoudness.kr(signal)[0];
		var threshold = -40;
		var is_predicting = loudness >= threshold;
		var trigger = Impulse.kr(30);
		FluidKrToBuf.kr(mfccs, ~mfccbuf);
		~neural_network.kr(trigger * is_predicting, ~mfccbuf, ~predictions);
		SendReply.kr(trigger * is_predicting, "/prediction", FluidBufToKr.kr(~predictions));
		SendReply.kr(trigger * (1 - is_predicting), "/prediction", -1);
		signal.dup;
	}.play;
};
)

// run it with trombone test sounds...
~server_predictions.value(~trombone_test);
// ... and with oboe test sounds
~server_predictions.value(~oboe_test);

// one could also query on the language side and get the label back as symbol
~analyze_and_play.value(~trombone_test);
~analyze_and_play.value(~oboe_test);

// execute the code below
(
var post_label = {
	arg label;
	label.postln;
}
~neural_network.predictPoint(~mfccbuf, post_label);
)