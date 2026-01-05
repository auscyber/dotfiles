{
  inputs,
  config,
  pkgs,
  ...
}:
{
  home-manager.useGlobalPkgs = true;
  home-manager.backupFileExtension = "backup";

  home-manager.sharedModules = [
    ../../home/default.nix
#    {
#      age.rekey.hostPubkey = config.age.rekey.hostPubkey;
#    }

  ];
  home-manager.useUserPackages = true;
  home-manager.extraSpecialArgs = {
    inherit inputs;
    inherit (inputs) nix-colors;
  };
}
