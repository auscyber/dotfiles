{ config, pkgs, ... }:
let
  cfg = config.auscybernix.systemPath;
in
{

  nix.channel.enable = true;
  nix.nixPath = [
    "nixpkgs=${pkgs.path}"
    { "darwin-config" = "${cfg}"; }
    { nixos-config = "${cfg}"; }
  ];

}
