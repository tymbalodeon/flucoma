(
~load_theme = {
    arg theme;
    if (theme == "gruvbox") {
        ~background = Color.fromHexString("#282828");
        ~highlight_background = Color.fromHexString("#504945");
        ~foreground = Color.fromHexString("#fbf1c7");
        ~red = Color.fromHexString("#cc241d");
        ~green = Color.fromHexString("#98971a");
        ~yellow = Color.fromHexString("#d79921");
        ~blue = Color.fromHexString("#458588");
        ~purple = Color.fromHexString("#b16286");
        ~aqua = Color.fromHexString("#689d6a");
        ~orange = Color.fromHexString("#d65d0e");
        ~gray = Color.fromHexString("#7f7061");
    } {
        ~background = nil;
        ~highlight_background = nil;
        ~foreground = nil;
        ~red = nil;
        ~green = nil;
        ~yellow = nil;
        ~blue = nil;
        ~purple = nil;
        ~aqua = nil;
        ~orange = nil;
        ~gray = nil;
    };
};
)