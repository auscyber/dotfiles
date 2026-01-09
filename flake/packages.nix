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
    { pkgs, system,... }:
    {
	packages = import ../overlays/literal.nix { inherit pkgs system inputs; };
    };
}
