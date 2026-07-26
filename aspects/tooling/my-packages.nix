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
  # x86_64-linux/aarch64-darwin already come from aspects/framework/flake-file.nix;
  # aarch64-linux is added here for lora-pi's perSystem dev outputs (see the
  # `supported` set in aspects/tooling/ci.nix). x86_64-freebsd was here too, but
  # nothing needs it (ci.nix already special-cased it as unsupported/no hosts) —
  # every perSystem across the ~40 aspects that declare one was evaluating for
  # that extra, unused system for nothing.
  systems = [
    "aarch64-linux"
  ];
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
