{ nixpkgs,  config, overlays, inputs, nixos-mailserver, ... }:
nixpkgs.lib.nixosSystem {
  system = "x86_64-linux";
  modules = [
    ./configuration.nix
    ./hardware-configuration.nix
    ./../../modules/system/grub.nix
    ./boot.nix
    nixos-mailserver.nixosModule
    ./mailserver.nix
    ./minecraft.nix
    {
#      home-manager = {
#        useGlobalPkgs = true;
#        useUserPackages = true;
#        users.auscyber = import ../../users/auscyber;
#      };
      nixpkgs = { inherit config overlays; };

    }
#    home-manager.nixosModules.home-manager
  ];

}

