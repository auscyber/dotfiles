{ inputs, ... }: {
  den.aspects.disko = {
    # nixos-only, and the inputs are declared on the aspect so that follows from
    # the aspect rather than from a path list.
    layers = [ "nixos" ];

    # Declared straight onto the aspect in the `inputs` class -- no `flake-file`
    # wrapper, no `ff` alias. The collector resolves this class per HOST, so
    # which layer these land in follows from which hosts pull the aspect in.
    inputs.disko.url = "github:nix-community/disko";
    inputs.disko.inputs.nixpkgs.follows = "nixpkgs";
    inputs.disko-zfs = {
      url = "github:numtide/disko-zfs";
      inputs = {
        disko.follows = "disko";
        flake-parts.follows = "flake-parts";
        nixpkgs.follows = "nixpkgs";
      };
    };

    # A module FUNCTION: den's effect trampoline deepSeqs its handler state at
    # every step, so a bare attrset here is fully forced during fleet
    # resolution, dragging these nixos-layer inputs into darwin evaluations too.
    nixos = _: {
      imports = [
        # Base disko: declarative partitioning, generates `fileSystems.*` from
        # `disko.devices`.
        inputs.disko.nixosModules.disko
        # numtide disko-zfs: optional runtime ZFS dataset-property reconciler
        # (option `disko.zfs`). Inert unless `disko.zfs.enable = true`.
        inputs.disko-zfs.nixosModules.default
      ];
    };
  };
}
