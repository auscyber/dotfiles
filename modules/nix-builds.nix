# Nix distributed builds — a single platform-guarded implementation in
# _common/builders-platform handles both NixOS and nix-darwin.
# The option definitions live in _common/builders.
# Both modules are registered under generic so they appear in both platforms.
{ ... }:
{
  flake.modules.generic = {
    nix-builds-options  = ../_common/builders/default.nix;
    nix-builds-platform = ../_common/builders-platform/default.nix;
  };
}
