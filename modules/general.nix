# General OS settings — a single file handles both NixOS and nix-darwin using
# platform guards (pkgs.stdenv.hostPlatform.isLinux / .isDarwin) internally.
# Registered under generic so it is included in both NixOS and nix-darwin.
{ ... }:
{
  flake.modules.generic.general = ../_common/general/default.nix;
}
