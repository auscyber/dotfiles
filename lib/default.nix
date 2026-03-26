inp@{
  inputs,
  ...
}:

let
  self = inputs.self;
  inherit (inputs.nixpkgs.lib) mapAttrs mkOption types;
  inherit (inputs.flake-parts.lib) mkSubmoduleOptions;
  builderConfig = types.submodule (submod: {
    options = {
      publicHostKey = mkOption {
        type = types.str;
        description = "Public SSH host key of the build machine.";
      };
      username = mkOption {
        type = types.str;
        default = "builder";
        description = "Username for the build machine.";
      };
      name = mkOption {
        type = types.str;
        default = "";
        description = "Name identifier for the build machine.";
      };
      hostname = mkOption {
        type = types.str;
        default = "";
        description = "Hostname of the build machine.";
      };
      ipAddress = mkOption {
        type = types.str;
        default = "";
        description = "IP Address of the build machine.";
      };
      systems = mkOption {
        type = types.listOf types.str;
        default = [ ];
        description = "List of systems this build machine can build for.";
      };
      maxJobs = mkOption {
        type = types.int;
        default = 1;
        description = "Maximum number of concurrent jobs this build machine can handle.";
      };
      absoluteSpeedFactor = mkOption {
        type = types.int;
        default = 1;
        description = "Speed factor for this build machine.";
      };
      features = mkOption {
        type = types.listOf types.str;
        default = [ ];
        description = "List of supported features for this build machine.";
      };

    };
  });
in
{

  config = {
    flake.lib = {
      system = import ./systems {
        inherit inputs;
        # Derive module lists from flake.modules.* so every feature module
        # registered via the dendritic pattern is automatically included.
        # Modules under flake.modules.generic are automatically merged into
        # both the NixOS and nix-darwin module lists, eliminating the need to
        # register the same module twice when it applies to both OS types.
        importedNixosModules  = builtins.attrValues (
          (inp.config.flake.modules.generic or { }) // (inp.config.flake.modules.nixos or { })
        );
        importedDarwinModules = builtins.attrValues (
          (inp.config.flake.modules.generic or { }) // (inp.config.flake.modules.darwin or { })
        );
        importedHomeModules   = builtins.attrValues (inp.config.flake.modules.homeManager or { });
        standaloneHomeModules   = inp.config.flake.auscybernix.standaloneHomeModules;
        config = inp.config;
      };
      file = import ./file.nix {
        inherit inputs;
        self = ../.;
      };
      overlay = import ./overlay.nix { inherit inputs; };
      extra = import ./extra.nix { inherit inputs; };
    };
  };
  options = {
    flake.auscybernix = mkSubmoduleOptions {
      # Module lists are now derived from flake.modules.* in config.
      # Only standalone-specific home-manager modules (not embedded) remain here.
      standaloneHomeModules = mkOption {
        type = types.listOf types.unspecified;
        default = [ ];
        description = "Home-manager modules only used in standalone (non-embedded) configurations.";
      };
      systems = mkOption {

        type = types.attrsOf types.unspecified;
        default =
          inputs.self.homeConfigurations
          // inputs.self.nixosConfigurations
          // inputs.self.darwinConfigurations;
      };
      builders = {
        sshKeys = mkOption {
          type = types.listOf types.str;
          default = [ ];
          description = "List of SSH public keys for accessing build machines.";
        };
        extraBuildMachines = mkOption {
          type = types.attrsOf (builderConfig);
          default = { };
        };
        buildMachines = mkOption {
          type = types.attrsOf (
            builderConfig

          );
          default = [ ];
        };

      };
      vpn = {
        configMap = mkOption {
          type = types.attrsOf (
            types.submodule (submod: {
              options = {
                description = mkOption {
                  type = types.str;
                  default = "";
                };
                ipAddress = mkOption {
                  type = types.str;
                  default = "";
                };
                pubkey = mkOption {
                  type = types.str;
                  default = "";
                };
              };
            })
          );
          default = { };
        };
      };

    };

  };

}
