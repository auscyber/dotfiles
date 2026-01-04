{
  inputs,
  ...
}:
{
  imports = [

    inputs.nixos-hardware.nixosModules.microsoft-surface-common
    ./configuration.nix
  ];

}
