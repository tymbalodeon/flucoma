(
~reset = {
    arg meter = true, plot_tree = false;
    s.freeAll;
    Window.closeAll;
    Buffer.freeAll;
    if (meter == true) {
        ServerMeter(s, numIns: 2, numOuts: 2);
    };
    if (plot_tree == true) {
        s.plotTree;
    };
};

~set_button = {
    arg bounds, states, action;
    states.do {
        arg state, index;
        if (state.size == 1) {
            state = state ++ [~background, ~foreground];
            states.put(index, state);
        };
    };
    Button(~window, bounds).states_(states).action_(action);
};
)