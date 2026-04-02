{ den, ... }:
let
  # Enable <angle-bracket> syntax for den aspect references in this file.
  __findFile = den.lib.__findFile;
in
{
  den.aspects.ivypierlot = {
    # Base home-manager configuration shared across all hosts where ivypierlot
    # lives.  Per-host additions are delivered by each host's provides.ivypierlot
    # (see e.g. flake/aspects/ivys-macbook-pro.nix).
    homeManager =
      { pkgs, ... }:
      {
        home = {
          username = "ivypierlot";
          homeDirectory = "/Users/ivypierlot";
        };
        programs.home-manager.enable = true;
      };

    # Feature aspects included for all ivypierlot homes (Darwin hosts).
    includes = [
      <shell>
      <editors>
      <browsers>
      <terminal>
      <gpg>
      <keybinds>
      <wm-darwin>
      <media>
    ];
  };
}
