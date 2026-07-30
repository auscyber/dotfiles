{
  den,
  inputs,
  ...
}:
{
  den.aspects.noctalia = { user, ... }: {
    # Declared on the aspect, not the file: the partition generator reads
    # which aspect owns an input, and which platforms pull that aspect in.
    flake-file = _: {
      inputs.noctalia = {
        url = "github:noctalia-dev/noctalia";
        inputs.nixpkgs.follows = "nixpkgs"; # this line is optional, prevents downloading two versions of nixpkgs but disables cache
      };
    };

    homeManager = {
      imports = [
        inputs.noctalia.homeModules.default
      ];
      programs.noctalia = {
        enable = true;
        settings = {
          theme = {
            mode = "dark";
            source = "builtin";
            builtin = "Catppuccin";
          };

          wallpaper = {
            enabled = true;
            default.path = "/path/to/wallpapers/wallpaper.png";
          };
        };
      };
    };
  };
}
