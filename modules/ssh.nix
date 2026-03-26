# SSH — the system SSH daemon lives on NixOS; the SSH client config lives in
# Home Manager. Both are part of the same conceptual "ssh" feature.
{ ... }:
{
  flake.modules = {
    nixos.ssh-service      = ../_nixos/ssh/default.nix;
    homeManager.ssh-client = ../_home/ssh/default.nix;
  };
}
