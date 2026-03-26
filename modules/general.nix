# General OS settings — combines NixOS and nix-darwin general configuration.
# Both platforms share the concept of: NH env variable, reload alias, nix.optimise.
{ ... }:
{
  flake.modules = {
    nixos.general  = ../_nixos/general/default.nix;
    darwin.general = ../_darwin/general/default.nix;
  };
}
