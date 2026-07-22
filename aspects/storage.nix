{ inputs, ... }:
{
  ff.disko.url = "github:nix-community/disko";
  ff.disko.inputs.nixpkgs.follows = "nixpkgs";
  ff.disko-zfs = {
    url = "github:numtide/disko-zfs";
    inputs = {
      disko.follows = "disko";
      flake-parts.follows = "flake-parts";
      nixpkgs.follows = "nixpkgs";
    };
  };

  den.aspects.disko.nixos = {
    imports = [
      # Base disko: declarative partitioning, generates `fileSystems.*` from
      # `disko.devices`.
      inputs.disko.nixosModules.disko
      # numtide disko-zfs: optional runtime ZFS dataset-property reconciler
      # (option `disko.zfs`). Inert unless `disko.zfs.enable = true`.
      inputs.disko-zfs.nixosModules.default
    ];
  };
}
