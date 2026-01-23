inp@{
  inputs,
  ...
}:

let
  self = inputs.self;
  inherit (inputs.nixpkgs.lib) mapAttrs mkOption types;
  inherit (inputs.flake-parts.lib) mkSubmoduleOptions;
in
{

  config = {
    flake.lib = {
      system = import ./systems {
        inherit inputs;
        inherit (self.auscybernix)
          importedDarwinModules
          importedNixosModules
          importedHomeModules
          standaloneHomeModules
          ;
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
      importedDarwinModules = mkOption {
        type = types.listOf types.unspecified;
        default = [ ];

      };
      importedNixosModules = mkOption {
        type = types.listOf types.unspecified;
        default = [ ];
      };
      importedHomeModules = mkOption {
        type = types.listOf types.unspecified;
        default = [ ];
      };
      standaloneHomeModules = mkOption {
        type = types.listOf types.unspecified;
        default = [ ];
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
        buildMachines = mkOption {
          type = types.attrsOf (
            types.submodule (submod: {
              options = {
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
                speedFactor = mkOption {
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
            })
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
