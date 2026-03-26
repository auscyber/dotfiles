# Nix distributed builds — options live in _common/builders, platform
# implementations live in _nixos/builders and _darwin/builders.
{ ... }:
{
  flake.modules = {
    nixos = {
      nix-builds-options  = ../_common/builders/default.nix;
      nix-builds-platform = ../_nixos/builders/default.nix;
    };
    darwin = {
      nix-builds-options  = ../_common/builders/default.nix;
      nix-builds-platform = ../_darwin/builders/default.nix;
    };
  };
}
