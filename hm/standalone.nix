{
  config,
  pkgs,
  ...
}:
{
  nix.package = pkgs.nixVersions.latest;
}
