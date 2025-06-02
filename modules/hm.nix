{
  inputs,
  config,
  pkgs,
  ...
}:
{
  home-manager.useGlobalPkgs = true;
  home-manager.sharedModules = [ ../hm/default.nix ];
  home-manager.useUserPackages = true;
  home-manager.extraSpecialArgs = { inherit inputs; };
}
