{
  inputs,
  den,
  ...
}:
{
  ff.nix-cachyos-kernel = {
    url = "github:xddxdd/nix-cachyos-kernel/release";
  };
  # Do not override its nixpkgs input, otherwise there can be mismatch between patches and kernel version

  den.aspects.cachyos-kernel = {
    includes = [ den.aspects.ccache ];
    overlays.cachyosKernels = inputs.nix-cachyos-kernel.overlays.pinned;
    nixos =
      {
        pkgs,
        config,
        lib,
        ...
      }:
      {
        boot.kernelPackages =
          let
            cachyOsPackages = import "${inputs.nix-cachyos-kernel.inputs.nixpkgs}" {
              system = pkgs.stdenv.system;
            };
            helpers = pkgs.callPackage "${inputs.nix-cachyos-kernel.outPath}/helpers.nix" { };
            #            stdenv = pkgs.ccacheStdenv.override { stdenv = helpers.stdenvLLVM; };
            stdenv = helpers.stdenvLLVM;

            kernel = pkgs.cachyosKernels.linux-cachyos-latest.override {
              lto = "thin";
              cpusched = "bore";
              stdenv = stdenv;
              extraMakeFlags = [
                "CC=${stdenv.cc}/bin/clang"
                "HOSTCC=${stdenv.cc}/bin/clang"
              ];
            };
          in
          (cachyOsPackages.linuxKernel.packagesFor kernel).extend (
            lib.composeManyExtensions (
              pkgs.kernelPackagesExtensions
              ++ [
                (kFinal: _: {
                  zfs_cachyos = (
                    kFinal.callPackage "${inputs.nix-cachyos-kernel.outPath}/zfs-cachyos/default.nix" {
                      inputs = { inherit (inputs) nixpkgs; };
                      # Which key to read out of zfs-cachyos/version.json. Upstream's
                      # packages.nix passes the kernel's `zfsVariant` passthru here; the
                      # arg's own default ("latest") is not a key in that file.
                      variant = kernel.zfsVariant;
                    }
                  );
                })
              ]
            )
          );

        boot.supportedFilesystems.zfs = true;
        boot.zfs.package = config.boot.kernelPackages.zfs_cachyos;
      };
  };
}
