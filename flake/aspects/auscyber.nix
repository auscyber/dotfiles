{ ... }:
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
      };
  };
}
