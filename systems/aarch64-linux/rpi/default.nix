{
  config,
  pkgs,
  inputs,
  ...
}:
{
  imports =
    with inputs.nixos-raspberrypi.nixosModules;
    [
      raspberry-pi-5.base
      raspberry-pi5.bluetooth
    ]
    ++ [
      ./configuration.nix
      ./hardware-configuration.nix
    ];

}
