# Secrets storage configuration — a single platform-guarded implementation in
# _common/secrets-platform covers NixOS, nix-darwin, and Home Manager.
# The option definitions live in _common/secrets.nix.
{ ... }:
{
  flake.modules = {
    nixos = {
      secrets-options  = ../_common/secrets.nix;
      secrets-platform = ../_common/secrets-platform/default.nix;
    };
    darwin = {
      secrets-options  = ../_common/secrets.nix;
      secrets-platform = ../_common/secrets-platform/default.nix;
    };
    homeManager = {
      secrets-options  = ../_common/secrets.nix;
      secrets-platform = ../_home/secrets/default.nix;
    };
  };
}
