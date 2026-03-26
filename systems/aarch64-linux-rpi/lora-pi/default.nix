{
  config,
  pkgs,
  inputs,
  nixos-raspberrypi,
  ...
}:
{
  auscybernix.modules.enable = {
    nix              = true;
    allConfigs       = true;
    ext-home-manager = true;
    ext-agenix       = true;
    ext-agenix-rekey = true;
    ext-sops         = true;
  };

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
