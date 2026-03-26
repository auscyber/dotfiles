# NixOS-only feature modules (no Darwin/Home Manager counterpart).
# Cross-cutting features (general, builders, secrets, ssh) are in their
# own top-level combined modules.
{ ... }:
{
  flake.modules.nixos = {
    nixos-bootlogo = ../_nixos/bootlogo/default.nix;
    nixos-games    = ../_nixos/games/default.nix;
  };
}
