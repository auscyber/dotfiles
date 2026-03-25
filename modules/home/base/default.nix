# Base home-manager modules: the primary entry point plus fundamental
# per-user modules (file management, GPG, SSH, secrets, standalone, …).
{ ... }:
{
  flake.modules.homeManager = {
    hm-default    = ../_home/default.nix;
    hm-file       = ../_home/file/default.nix;
    hm-gpg        = ../_home/gpg/default.nix;
    hm-secrets    = ../_home/secrets/default.nix;
    hm-ssh        = ../_home/ssh/default.nix;
    hm-standalone = ../_home/standalone/default.nix;
  };
}
