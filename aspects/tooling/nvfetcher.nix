# The nvfetcher sources + post-fetch `script` mechanism now lives in the shared
# `ivixlib` repo, delivered as a flake-parts flakeModule plus the denful `ivixlib`
# namespace. dendritic consumes it here for its OWN sources (proton-ge-bin,
# pam_rssh, vscode-kanata, …); ivylix consumes the exact same mechanism — so both
# the dotfiles and ivylix share ivixlib.
#
# This replaces the ~300-line local copy (options.nvfetcher.sources, the `sources`
# module-arg battery, withExtra, and the update-sources/postprocess-sources apps),
# and drops the local `patchedInputs.nvfetcher` — ivixlib builds the forked
# "custom fetcher" nvfetcher itself.
{ inputs, ... }:
{
  imports = [
    inputs.ivixlib.flakeModules.default
    (inputs.den.namespace "ivixlib" inputs.ivixlib)
  ];
}
