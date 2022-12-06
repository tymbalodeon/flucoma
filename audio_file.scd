(
~get_audio_file = {
    arg filename, start_frame = 0, num_frames = -1;
    var path = FluidFilesPath(filename);
    Buffer.read(s, path, start_frame, num_frames);
};

~get_audio_files = {
    arg file_names, start_frame = 0, num_frames = -1;
    var number_of_files = file_names.size;
    var audio_files = Array(maxSize: number_of_files);
    file_names.do {
        arg file_name;
        var audio_file = ~get_audio_file.(
            file_name,
            start_frame,
            num_frames
        );
        audio_files.add(audio_file);
    };
    audio_files;
};
)