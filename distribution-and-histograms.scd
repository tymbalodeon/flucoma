(
~display_distributions = {
    arg data_set, number_of_bins = 100;
    data_set.dump {
        arg dictionary;
        var data = dictionary["data"].values.flop;
        var histograms = data.collect {
            arg dimensions;
            dimensions.histo(number_of_bins);
        };
        var make_window = {
            var window = Window(
                name: "Distributions",
                bounds: Rect(0, 0, 800, 820),
            ).front;
            var window_width = window.bounds.width;
            var plotter_height = window.bounds.height - 20;
            var plotter = Plotter(
                name: "Distributions",
                bounds: Rect(0, 20, window_width, plotter_height),
                parent: window
            );
            EZSlider(
                parent: window,
                bounds: Rect(0, 0, window_width, 20),
                label: "Dimension:",
                controlSpec: ControlSpec(
                    minval: 0,
                    maxval: histograms.size - 1,
                    step: 1
                ),
                action: {
                    arg slider;
                    plotter.value_(histograms[slider.value.asInteger]);
                },
                initVal: 0,
                initAction: true,
                labelWidth: 80
            );
            ~distributions_window = window;
        };
        fork(make_window, AppClock);
    };
};
)