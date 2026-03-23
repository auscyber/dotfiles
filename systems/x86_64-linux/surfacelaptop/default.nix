{
  inputs,
  ...
}:
{
  imports = [

    inputs.nixos-hardware.nixosModules.microsoft-surface-common
    inputs.nixos-hardware.nixosModules.microsoft-surface-laptop-amd
    ./configuration.nix
  ];

}
