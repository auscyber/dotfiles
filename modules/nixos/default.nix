# NixOS-specific feature modules.
{ ... }:
{
  flake.modules.nixos = {
    nixos-bootlogo  = ../_nixos/bootlogo/default.nix;
    nixos-builders  = ../_nixos/builders/default.nix;
    nixos-games     = ../_nixos/games/default.nix;
    nixos-general   = ../_nixos/general/default.nix;
    nixos-secrets   = ../_nixos/secrets/default.nix;
    nixos-ssh       = ../_nixos/ssh/default.nix;
  };
}
