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
      };
      file = import ./file.nix {
        inherit inputs;
        self = ../.;
      };
      overlay = import ./overlay.nix { inherit inputs; };
      extra = import ./extra.nix { inherit inputs; } // inp;
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

    };

  };

}
