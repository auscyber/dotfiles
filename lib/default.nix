inp@{
  inputs,
  ...
}:

let
  self = inputs.self;
  inherit (inputs.nixpkgs.lib) mapAttrs mkOption types;
  inherit (inputs.flake-parts.lib) mkSubmoduleOptions;

  # Convert a named module map { NAME = path; ... } into a single Nix module
  # that:
  #   1. Declares options.auscybernix.modules.enable as an attrsOf bool.
  #   2. Imports each `path` only when
  #      `config.auscybernix.modules.enable.NAME` is not explicitly `false`.
  #
  # This lets any system configuration opt out of a registered feature module
  # by its NAME label — the same label used in flake.modules.<kind>.NAME:
  #
  #   auscybernix.modules.enable = { "nixos-games" = false; };
  #
  # Modules whose NAME is absent from the attrset are enabled by default.
  mkSelectableModules =
    moduleMap:
    { config, lib, ... }:
    {
      options.auscybernix.modules.enable = mkOption {
        type = types.attrsOf types.bool;
        default = { };
        description = ''
          Per-name enable flags for feature modules registered via
          flake.modules.<kind>.NAME.  Modules not listed here default to
          enabled.  Set NAME = false to exclude a module from this
          system's evaluation.

          Available names are the keys of flake.modules.nixos,
          flake.modules.darwin, flake.modules.generic, and
          flake.modules.homeManager.

          Example:
            auscybernix.modules.enable = {
              "nixos-games"    = false;
              "nixos-bootlogo" = false;
            };
        '';
      };

      imports = builtins.concatLists (
        inputs.nixpkgs.lib.mapAttrsToList (
          name: path: inputs.nixpkgs.lib.optional (config.auscybernix.modules.enable.${name} or true) path
        ) moduleMap
      );
    };

  # Named module maps (merged at flake-evaluation time, before any system
  # evaluation).  Generic modules are included in both OS families.
  nixosModuleMap = (inp.config.flake.modules.generic or { }) // (inp.config.flake.modules.nixos or { });
  darwinModuleMap = (inp.config.flake.modules.generic or { }) // (inp.config.flake.modules.darwin or { });
  homeModuleMap = inp.config.flake.modules.homeManager or { };

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
        # Each mkSelectableModules call produces one wrapper module that
        # defines options.auscybernix.modules.enable and conditionally imports
        # every registered path based on that option's per-name bool values.
        importedNixosModules  = [ (mkSelectableModules nixosModuleMap) ];
        importedDarwinModules = [ (mkSelectableModules darwinModuleMap) ];
        importedHomeModules   = [ (mkSelectableModules homeModuleMap) ];
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
