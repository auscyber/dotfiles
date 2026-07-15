{
  inputs,
  lib,
  ...
}:
# The option sources our searchix instance serves (see ./_searchix-sources.nix).
# Exposed as a package so they can be inspected without deploying:
#   nix build .#searchix-option-sources && jq 'keys | length' result/dendritic/options.json
{
  perSystem = { pkgs, ... }: {
    packages.searchix-option-sources =
      (import ./_searchix-sources.nix { inherit lib pkgs inputs; }).tree;
  };
}
