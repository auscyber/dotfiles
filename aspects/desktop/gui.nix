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
          dur_file_path = builtins.toString (
            builtins.path {
              name = "blackhole.dur";
              path = ./blackhole.dur;
            }
          );
          full_color = true;
        };
      };
    };
  };

  den.aspects.razer = {
    gui.nixos = { user, pkgs, ... }: {
      hardware.openrazer.enable = true;
      users.users.${user.name}.extraGroups = [ "openrazer" ];

    };

  };
  den.aspects.auspc.includes = [ den.aspects.razer ];
}
