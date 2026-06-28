{
  lib,
  withSystem,
  inputs,
  ...
}:

{

  imports =
    with inputs.nixpkgs.lib;
    ../../packages
    |> fileset.fileFilter (
      file: file.hasExt "nix" && !hasPrefix "package" file.name && !hasPrefix "_" file.name
    )
    |> fileset.toList;

}
