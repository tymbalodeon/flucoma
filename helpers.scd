(
~reset = {
    Window.closeAll;
    Buffer.freeAll;
    s.freeAll;
    ServerMeter(s, numIns: 2, numOuts: 2);
};

~set_button = {
    arg bounds, states, action;
    Button(~window, bounds).states_(states).action_(action);
};
)