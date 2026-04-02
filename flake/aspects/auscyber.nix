{ den, ... }:
let
  # Enable <angle-bracket> syntax for den aspect references in this file.
  __findFile = den.lib.__findFile;
in
{
  den.aspects.auscyber = {
    # Base home-manager configuration shared across all hosts where auscyber
    # lives.  Per-host additions are delivered by each host's provides.auscyber
    # (see e.g. flake/aspects/auspc.nix).
    homeManager =
      { pkgs, ... }:
      {
        home = {
          username = "auscyber";
          homeDirectory = "/home/auscyber";
        };
        programs.home-manager.enable = true;
      };

    # Feature aspects included for all auscyber homes (Linux hosts).
    includes = [
      <shell>
      <editors>
      <browsers>
      <terminal>
      <gpg>
      <keybinds>
      <media>
      <languages>
      <wm-linux>
    ];
  };
}
