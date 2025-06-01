{
  nixpkgs,
  config,
  overlays,
  inputs,
  nixos-mailserver,
  home-manager,
  home-manager-modules,
  modules,
}:
nixpkgs.lib.nixosSystem {
  system = "x86_64-linux";
  specialArgs = { inherit inputs; };
  modules = modules ++ [
    ./configuration.nix
    ./hardware-configuration.nix
    #    ./../../modules/system/grub.nix
    ./boot.nix
    nixos-mailserver.nixosModule
    #    ./mailserver.nix
    #    ./minecraft.nix
    home-manager.nixosModules.home-manager
    {

      home-manager.users.auscyber = {
        imports = home-manager-modules;
        home.username = "auscyber";
        home.sessionVariables = {
          FLAKENAME = "auscyber";
          NIXFLAKE = "$HOME/dotfiles/nixos-config";
        };

      };
      nixpkgs = { inherit config overlays; };

    }
  ];

}
