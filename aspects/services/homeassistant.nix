{
  den,
  inputs,
  lib,
  ...
}:
{
  den.aspects.libvirt = {
    layers = [ "nixos" ];
    inputs.nixvirt = {
      url = "https://flakehub.com/f/AshleyYakeley/NixVirt/*.tar.gz";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # A module FUNCTION, like the other 49 class-content sites in this tree.
    #
    # den's effect trampoline deepSeqs its handler state at every step (fx
    # src/trampoline.nix: `k = builtins.deepSeq newState (step.key + 1)`, for
    # stack safety), so class content written as a bare attrset is fully forced
    # -- including an `inputs.<x>` buried in an `imports` list. `deepSeq` on a
    # function forces the closure, not its body, so the function form defers it
    # to when this host actually builds. `nixvirt` is a nixos-layer input; left
    # as an attrset it is one aspect-inclusion away from being forced by darwin
    # evaluations too, which is what `checks.layer-isolation` catches.
    nixos = _: {
      imports = lib.optional (inputs ? nixvirt) inputs.nixvirt.nixosModules.default;
    };
  };

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
