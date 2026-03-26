# General OS settings — a single file handles both NixOS and nix-darwin using
# platform guards (pkgs.stdenv.hostPlatform.isLinux / .isDarwin) internally.
{ ... }:
{
  flake.modules = {
    nixos.general  = ../_common/general/default.nix;
    darwin.general = ../_common/general/default.nix;
  };
}
