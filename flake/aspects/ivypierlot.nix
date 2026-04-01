{ ... }:
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
      };
  };
}
