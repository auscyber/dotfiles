{ nixpkgs, config, overlays, inputs, agenix, ... }:
nixpkgs.lib.nixosSystem {
  system = "x86_64-linux";
  modules = [
    ./configuration.nix
    ./hardware-configuration.nix
    ./../../modules/system/grub.nix
    ./boot.nix
    agenix.nixosModules.age
    {
      environment.systemPackages = [ agenix.defaultPackage.x86_64-linux ];
    }
  ];

}

