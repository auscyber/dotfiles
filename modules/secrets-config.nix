# Secrets storage configuration — the option definitions live in
# _common/secrets.nix and the platform-specific storage/configId setup lives
# in the per-platform modules.
{ ... }:
{
  flake.modules = {
    nixos = {
      secrets-options  = ../_common/secrets.nix;
      secrets-platform = ../_nixos/secrets/default.nix;
    };
    darwin = {
      secrets-options  = ../_common/secrets.nix;
      secrets-platform = ../_darwin/security/secrets/default.nix;
    };
    homeManager = {
      secrets-options  = ../_common/secrets.nix;
      secrets-platform = ../_home/secrets/default.nix;
    };
  };
}
