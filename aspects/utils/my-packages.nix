{
  den,
  lib,
  ...
}:
let
  # Names declared under den.aspects.packages.<name> get exposed as flake outputs
  # at packages.<system>.my.<name> so each can be built individually with
  # `nix build .#my.<name>`. Skip entries that don't materialise as a top-level
  # `pkgs.<name>` derivation (e.g. eagle-nvim lives under vimPlugins, alx-wol
  # under kernelPackageExtensions, zotero-extensions is an attrset).
  declaredPackageNames = builtins.attrNames (den.aspects.packages or { });

  isBuildable = pkg: lib.isDerivation pkg;
in
{
  perSystem =
    { pkgs, ... }:
    let
      collected = lib.genAttrs declaredPackageNames (name: pkgs.${name} or null);
    in
    {
      # Use legacyPackages so we can nest under `my` — flake-parts' packages
      # output requires each entry to be a flat derivation. `nix build .#my.X`
      # still resolves through legacyPackages.
      legacyPackages.my = lib.filterAttrs (_: v: v != null && isBuildable v) collected;
    };
}
