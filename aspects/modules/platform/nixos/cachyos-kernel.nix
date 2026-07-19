{ inputs, ... }:
{

  ff.nix-cachyos-kernel = {
    url = "github:xddxdd/nix-cachyos-kernel/release";
  };
  # Do not override its nixpkgs input, otherwise there can be mismatch between patches and kernel version

  den.aspects.cachyos-kernel = {
    nixos = { pkgs, config, ... }: {
      boot.kernelPackages = pkgs.cachyosKernels.linuxPackages-cachyos-bore;
      imports = [ inputs.nix-cachyos-kernel.overlays.pinned ];

      boot.supportedFilesystems.zfs = true;
      boot.zfs.package = config.boot.kernelPackages.zfs_cachyos;
    };

  };

}
