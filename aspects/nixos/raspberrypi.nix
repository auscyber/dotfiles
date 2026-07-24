{
  den,
  inputs,
  lib,
  ...
}:
let
in
{
  ff = {
    nixos-raspberrypi = {
      url = "github:nvmd/nixos-raspberrypi/main";
      # Upstream pins nixos-25.11; follow root nixpkgs so stylix (which targets
      # 26.11) can define options like services.displayManager.generic here.
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixpkgs-nvmd.url = "github:nvmd/nixpkgs/modules-with-keys-25.11";
    nixos-images = {
      url = "github:nvmd/nixos-images/sdimage-installer";
      inputs.nixos-stable.follows = "nixpkgs-nvmd";
      inputs.nixos-unstable.follows = "nixpkgs-nvmd";
    };
  };

  den.lib.raspberry-pi-builder =
    args:
    inputs.nixos-raspberrypi.lib.nixosSystem (
      {
        system = "aarch64-linux";
        specialArgs = {
          inherit (inputs) self;
          inherit inputs;
        };
      }
      // args
    );

  den.aspects.nixos-raspberrypi = {
    nixos = {
      imports = with inputs.nixos-raspberrypi.nixosModules; [
        raspberry-pi-5.base
        raspberry-pi-5.bluetooth
        raspberry-pi-5.page-size-16k
      ];
      _module.args.nixos-raspberrypi = inputs.nixos-raspberrypi;
    };
  };

  # RPI hosts must be evaluated with nixos-raspberrypi.lib.nixosSystem rather
  # than nixpkgs.lib.nixosSystem (dotfiles/lib/systems/mk-rpi.nix). den's default
  # builder is nixpkgs's nixosSystem; for any host registered under the custom
  # `aarch64-linux-rpi` system, swap the builder by overriding flake.nixosConfigurations.
}
