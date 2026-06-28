{ den, ... }:
{
  den.aspects.nixos-general = {
    nixos =
      { ... }:
      {
        nix.optimise = {
          automatic = true;
          dates = [ "03:45" ];
        };
        environment.shellAliases.re = "nh os switch";
      };
  };

  den.schema.host.includes = [
    den.aspects.nixos-general
    den.aspects.openssh
  ];
}
