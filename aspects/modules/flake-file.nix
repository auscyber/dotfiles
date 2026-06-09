{
  inputs,
  lib,
  config,
  ...
}:
{
  imports = [
    inputs.flake-file.flakeModules.default
    (lib.mkAliasOptionModule [ "ff" ] [ "flake-file" "inputs" ])
  ];

  disabledModules = [ (inputs.flake-file + "/modules/flake-parts.nix") ];

  perSystem =
    { pkgs, ... }:
    {
      apps =
        config.flake-file.apps
        |> lib.mapAttrs (
          _: f:
          let
            pkg = f pkgs;
          in
          {
            type = "app";
            program = lib.getExe pkg;
          }
        );

      checks.check-flake-file = config.flake-file.check-flake-file pkgs;
    };

  flake-file = {
    inputs.flake-file.url = "github:denful/flake-file";
    outputs = /* nix */ ''
      inputs:
      inputs.flake-parts.lib.mkFlake { inherit inputs; } {
        # Import all *.nix files in the ./aspects directory
        # Except ones that start with '_'
        imports =
          with inputs.nixpkgs.lib;
          ./aspects
          |> fileset.fileFilter (file: file.hasExt "nix" && !hasPrefix "_" file.name)
          |> fileset.toList;

        _module.args.rootPath = ./.;
      }
    '';

  };
}
