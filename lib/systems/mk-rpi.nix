{
  inputs,
}:
{
  system,
  hostname,
  modules ? [ ],
  ...
}:
let
  common = import ./common.nix { inherit inputs; };
  flake = inputs.self;
  extendedLib = (common.mkExtendedLib flake inputs.nixpkgs-nvmd);
  matchingHomes = common.mkHomeConfigs {
    inherit
      flake
      system
      hostname
      ;
  };
  homeManagerConfig = common.mkHomeManagerConfig {
    inherit
      extendedLib
      inputs
      system
      matchingHomes
      ;
    isNixOS = true;
  };
  specialArgs =
    common.mkSpecialArgs {
      inherit
        inputs
        hostname
        system
        extendedLib
        ;
    }
    // {
      nixos-raspberrypi = inputs.nixos-raspberrypi;
    };
  stylixConfig = {
    home-manager.sharedModules = [
      {
        imports = [ inputs.stylix.homeModules.stylix ];
        disabledModules = [
          "${inputs.stylix}/modules/qt/hm.nix"
        ];
      }
    ];

  };

in
{
  mkInstaller = inputs.nixos-raspberrypi.lib.nixosInstaller {
    inherit specialArgs;
    modules = [
      { _module.args.lib = extendedLib; }
      inputs.nixos-images.nixosModules.sdimage-installer
      stylixConfig
      (
        {
          config,
          lib,
          modulesPath,
          ...
        }:
        {
          disabledModules = [
            # disable the sd-image module that nixos-images uses
            (modulesPath + "/installer/sd-card/sd-image-aarch64-installer.nix")
          ];
          # nixos-images sets this with `mkForce`, thus `mkOverride 40`
          image.baseName =
            let
              cfg = config.boot.loader.raspberryPi;
            in
            lib.mkOverride 40 "nixos-installer-rpi${cfg.variant}-${cfg.bootloader}";
        }
      )

      inputs.stylix.nixosModules.stylix
      inputs.impermanence.nixosModules.impermanence
      inputs.home-manager.nixosModules.home-manager
      inputs.sops-nix.nixosModules.sops

      ../../modules/common/secrets.nix
      ../../modules/common/nix
      ../../modules/common/common
      {
        nixpkgs = {
          inherit system;

        }
        // common.mkNixpkgsConfig flake;
      }
      homeManagerConfig
    ]

    ++ (extendedLib.importModulesRecursive ../../modules/nixos)
    ++ [
      ../../systems/${system}/${hostname}
    ]
    ++ modules;

  };
  mkSystem = inputs.nixos-raspberrypi.lib.nixosSystem {
    inherit specialArgs;

    modules = [
      { _module.args.lib = extendedLib; }
      stylixConfig
      inputs.stylix.nixosModules.stylix
      inputs.impermanence.nixosModules.impermanence
      inputs.home-manager.nixosModules.home-manager
      inputs.sops-nix.nixosModules.sops

      {
        nixpkgs = {
          inherit system;

        }
        // common.mkNixpkgsConfig flake;
      }
      homeManagerConfig
    ]

    ++ (extendedLib.importModulesRecursive ../../modules/nixos)
    ++ [
      ../../systems/${system}/${hostname}
    ]
    ++ modules;

  };
}
