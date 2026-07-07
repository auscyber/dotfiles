{
  den,
  inputs,
  lib,
  ...
}:
{
  ff.nixvirt = {
    url = "https://flakehub.com/f/AshleyYakeley/NixVirt/*.tar.gz";
    inputs.nixpkgs.follows = "nixpkgs";
  };

  den.aspects.libvirt.nixos.imports = lib.optional (
    inputs ? nixvirt
  ) inputs.nixvirt.nixosModules.default;

  den.aspects.homeassistant = {
    includes = [ den.aspects.libvirt ];
    nixos = {
      virtualisation.libvirt = {
        enable = true;
      };
      networking.enableIPv6 = true;
      networking.bridges.br0.interfaces = [ "enp2s0" ];
      networking.interfaces.br0.useDHCP = false;
      networking.interfaces.br0.ipv4.addresses = [
        {
          address = "192.168.0.26";
          prefixLength = 24;
        }
      ];
      networking.defaultGateway = "192.168.0.1";
      networking.nameservers = [ "1.1.1.1" ];

    };

  };
}
