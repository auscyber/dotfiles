{ nixpkgs
, config
, overlays
, inputs
, home-manager
, home-manager-modules
, modules
, ...
}:
nixpkgs.lib.nixosSystem {
  specialArgs = { inherit inputs; };
  system = "x86_64-linux";
  modules = modules ++ [
    ./configuration.nix
    ./hardware-configuration.nix
    ./games.nix
    ./graphics.nix
    #./../../modules/system/grub.nix
    #./boot.nix
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
