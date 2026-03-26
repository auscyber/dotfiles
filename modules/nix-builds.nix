# Nix distributed builds — a single platform-guarded implementation in
# _common/builders-platform handles both NixOS and nix-darwin.
# The option definitions live in _common/builders.
{ ... }:
{
  flake.modules = {
    nixos = {
      nix-builds-options  = ../_common/builders/default.nix;
      nix-builds-platform = ../_common/builders-platform/default.nix;
    };
    darwin = {
      nix-builds-options  = ../_common/builders/default.nix;
      nix-builds-platform = ../_common/builders-platform/default.nix;
    };
  };
}
