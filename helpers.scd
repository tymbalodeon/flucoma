(
~reset = {
    arg plot_tree = false;
    s.freeAll;
    Window.closeAll;
    Buffer.freeAll;
    ServerMeter(s, numIns: 2, numOuts: 2);
    if (plot_tree == true) {
        s.plotTree;
    };
};

~set_button = {
    arg bounds, states, action;
    Button(~window, bounds).states_(states).action_(action);
};
)