{
  self,
  inputs,
  lib,
  ...
}:
{
  imports = [
  ];
  flake = {

  };
  perSystem =
    { pkgs, system, ... }:
    {
      packages =
        let
          packages = import ../overlays/literal.nix { inherit pkgs system inputs; };

        in
        pkgs.lib.filterAttrs (_: p: p ? meta && p.meta ? platforms) packages;

    };
}
