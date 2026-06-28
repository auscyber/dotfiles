{
  den,
  inputs,
  lib,
  ...
}:
let
  rpiSystem = "aarch64-linux-rpi";
  hasRpi = (inputs ? nixos-raspberrypi);

  rpiHosts = lib.attrByPath [ "hosts" rpiSystem ] { } den;
in
{
  flake-file.inputs = {
    nixos-raspberrypi.url = "github:nvmd/nixos-raspberrypi/main";
    nixpkgs-nvmd.url = "github:nvmd/nixpkgs/modules-with-keys-25.11";
    nixos-images = {
      url = "github:nvmd/nixos-images/sdimage-installer";
      inputs.nixos-stable.follows = "nixpkgs-nvmd";
      inputs.nixos-unstable.follows = "nixpkgs-nvmd";
    };
  };

  #  den.aspects.nixos-raspberrypi = lib.mkIf hasRpi {
  #    nixos = {
  #      imports = with inputs.nixos-raspberrypi.nixosModules; [
  #        raspberry-pi-5.base
  #        raspberry-pi-5.bluetooth
  #        raspberry-pi-5.page-size-16k
  #      ];
  #      _module.args.nixos-raspberrypi = inputs.nixos-raspberrypi;
  #    };
  #  };

  # RPI hosts must be evaluated with nixos-raspberrypi.lib.nixosSystem rather
  # than nixpkgs.lib.nixosSystem (dotfiles/lib/systems/mk-rpi.nix). den's default
  # builder is nixpkgs's nixosSystem; for any host registered under the custom
  # `aarch64-linux-rpi` system, swap the builder by overriding flake.nixosConfigurations.
  #  flake.nixosConfigurations = lib.mkIf hasRpi (
  #    lib.mapAttrs (
  #      name: host:
  #      inputs.nixos-raspberrypi.lib.nixosSystem {
  #        system = "aarch64-linux";
  #        specialArgs = {
  #          inherit (inputs) self;
  #          inherit inputs;
  #          nixos-raspberrypi = inputs.nixos-raspberrypi;
  #          hostname = name;
  #        };
  #        modules = host._modules or [ ];
  #      }
  #    ) rpiHosts
  #  );
}
