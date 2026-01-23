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
