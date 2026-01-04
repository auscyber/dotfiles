{
  self,
  inputs,
  lib,
  ...
}:
{

  perSystem =
    perSystemArgs@{ pkgs, ... }:

    let
      shellPath = ../shells;
      shellFiles = lib.filterAttrs (name: type: type == "regular" && lib.strings.hasSuffix name ".nix") (
        builtins.readDir shellPath
      );
      shellNames = lib.mapAttrsToList (name: _: lib.strings.removeSuffix name ".nix") shellFiles;
    in
    {

      devShells =
        (lib.foldl' (
          acc: name:
          let
            shell = import (shellPath + "/" + name + ".nix") {
              inherit pkgs;
              inherit (inputs.lib.devenv) mkShell;
            };
          in
          acc // { "${name}" = shell; }
        ) { } shellNames)
        // {
          default = pkgs.mkShell {
            buildInputs = with pkgs; [
              sops
              age
              ssh-to-age
            ] ++ perSystemArgs.config.input-branches.commands.all;
          };
        };

    };
}
