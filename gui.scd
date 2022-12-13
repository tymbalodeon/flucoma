(
~exit = {
    s.freeAll;
    Window.closeAll;
    Buffer.freeAll;
};

~reset = {
    arg meter = false, plot_tree = false;
    ~exit.();
    if (meter == true) {
        ServerMeter(s, numIns: 2, numOuts: 2);
    };
    if (plot_tree == true) {
        s.plotTree;
    };
};

~get_window = {
    arg name;
    Window(name, scroll: true).background_(~background).front;
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