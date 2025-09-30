{
  config,
  pkgs,
  inputs,
  nixos-raspberrypi,
  ...
}:
{
  imports =
    with nixos-raspberrypi.nixosModules;
    [
      raspberry-pi-5.base
      raspberry-pi-5.bluetooth
      raspberry-pi-5.page-size-16k

    ]
    ++ [
      ./configuration.nix
      ./hardware-configuration.nix
    ];

}
