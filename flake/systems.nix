{
  self,
  inputs,
  lib,
  ...
}:
let
  inherit (self.lib.file) parseSystemConfigurations filterRpiSystems;
  allSystems = parseSystemConfigurations ../systems;
in
{
  # Architectures that flake-parts builds perSystem attributes for.
  systems = [
    "aarch64-darwin"
    "x86_64-linux"
    "aarch64-linux"
  ];

  # Raspberry Pi SD-card installer images.  NixOS/darwin hosts are now
  # declared in flake/den.nix and built by den; only the non-standard RPi
  # installer flow stays here.
  flake.installImages = lib.mapAttrs' (
    name:
    { system, hostname, ... }:
    {
      name = hostname;
      value = (self.lib.system.rpi.mkInstaller { inherit inputs system hostname; }).config.system.build.sdImage;
    }
  ) (filterRpiSystems allSystems);
}
