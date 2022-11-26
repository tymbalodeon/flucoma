(
~load_theme = {
    arg theme;
    switch (
        theme,
        "gruvbox", {
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
        }, "lantern", {
            ~background = Color.fromHexString("#261b17");
            ~highlight_background = Color.fromHexString("#291812");
            ~foreground = Color.fromHexString("#e4cbb3");
            ~red = Color.fromHexString("#e01d1d");
            ~green = Color.fromHexString("#4d772d");
            ~yellow = Color.fromHexString("#fabc2c");
            ~blue = Color.fromHexString("#4395a3");
            ~purple = Color.fromHexString("#b666e6");
            ~aqua = Color.fromHexString("#f99666");
            ~orange = Color.fromHexString("#e35b15");
            ~gray = Color.fromHexString("#fab2c2");
        }, {
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
    );
};
)