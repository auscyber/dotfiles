{ nixpkgs, config, overlays, inputs, home-manager-modules, modules ? [], ... }:
nixpkgs.lib.nixosSystem {
  system = "x86_64-linux";
  modules = modules ++ [
    ./configuration.nix
    ./hardware-configuration.nix
    #./../../modules/system/grub.nix
    #./boot.nix
    #    ./mailserver.nix
    #    ./minecraft.nix
    {
      home-manager = {
        useGlobalPkgs = true;
        useUserPackages = true;
        users.auscyber = {
          imports = home-manager-modules;
          home.username = "auscyber";
          home.sessionVariables = {
            FLAKENAME = "auscyber";
            NIXFLAKE = "$HOME/dotfiles/nixos-config";
          };


        };
      };
      nixpkgs = { inherit config overlays; };

    }
  ];

}
