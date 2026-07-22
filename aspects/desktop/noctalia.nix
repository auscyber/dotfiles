{
  den,
  inputs,
  ...
}:
{
  ff.noctalia = {
    url = "github:noctalia-dev/noctalia";
    inputs.nixpkgs.follows = "nixpkgs"; # this line is optional, prevents downloading two versions of nixpkgs but disables cache
  };
  den.aspects.noctalia = { user, ... }: {
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
