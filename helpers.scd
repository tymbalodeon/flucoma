(
~reset_windows = {
    Window.closeAll;
    ServerMeter(s, 2, 2);
};

~set_button = {
    arg bounds, states, action;
    Button(~window, bounds).states_(states).action_(action);
};
)