{ den, ... }:
{
  den.aspects.karabiner-driver = {
    darwin =
      { pkgs, ... }:
      {
        services.karabiner-dk = {
          enable = true;
          package = pkgs.kanata.darwinDriver;
        };
      };
  };
}
