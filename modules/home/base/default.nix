# Base home-manager modules: the primary entry point plus fundamental
# per-user modules (file management, GPG, standalone).
# Cross-cutting features (secrets, ssh) are in their own top-level combined
# modules (secrets-config.nix, ssh.nix).
{ ... }:
{
  flake.modules.homeManager = {
    default    = ../_home/default.nix;
    file       = ../_home/file/default.nix;
    gpg        = ../_home/gpg/default.nix;
    standalone = ../_home/standalone/default.nix;
  };
}
