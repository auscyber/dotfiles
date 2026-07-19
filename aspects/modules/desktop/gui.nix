{ den, ... }: {
  den.aspects.gui = {
    includes = [
      den.aspects.onepassword
      den.aspects.ghostty
    ];
    gui.nixos = {
      services.displayManager.ly = {
        enable = true;
        settings = {
          animation = "dur_file";
          dur_file_path = builtins.toString ./blackhole.dur;
          full_color = true;
        };
      };
    };
  };
}
