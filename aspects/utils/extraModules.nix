{ inputs, lib, ... }:
let
  inherit (lib) fileset;

  byTopLevel =
    root:
    builtins.mapAttrs (
      name: type:
      if type == "directory" then
        fileset.toList (
          fileset.fileFilter (f: f.type == "regular" && f.hasExt "nix" && !(lib.hasPrefix "_" f.name)) (
            root + "/${name}"
          )
        )
      else
        null
    ) (builtins.readDir root);

  imported = builtins.mapAttrs (_class: importedFiles: { imports = importedFiles; }) (
    byTopLevel ../../extraModules
  );
in
{
  # Some extraModules (e.g. extraModules/homeManager/programs/zotero) need
  # `inputs` as a module argument. That injection is owned by patch-inputs.nix
  # (`home-manager.extraSpecialArgs.inputs`), which also merges in any patched
  # inputs — keeping a single definition avoids extraSpecialArgs merge clashes.
  den.default = imported;
}
