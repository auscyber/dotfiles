{
  inputs,
  den,
  lib,
  ...
}:
{
  den.aspects.cachyos-kernel = {
    # Declared on the aspect, not the file: the partition generator reads which
    # aspect owns an input, and hosts tell it which platforms pull that aspect
    # in. Do not override its nixpkgs input, otherwise there can be a mismatch
    # between patches and kernel version.
    inputs.nix-cachyos-kernel.url = "github:xddxdd/nix-cachyos-kernel/release";
    inputs.nix-cachyos-kernel.inputs.nixpkgs.follows = "nixpkgs";

    includes = [ den.aspects.ccache ];
    # Linux-guarded, and the guard has to be INSIDE the overlay function.
    #
    # `overlays` is routed into `_collectedOverlays` at the flake-parts level
    # with `collectSubtree = true` (../../tooling/overlays.nix), so it is
    # collected fleet-wide and applied to every system's `pkgs` -- including
    # aarch64-darwin, which has no use for a CachyOS kernel. Written bare, that
    # forced `inputs.nix-cachyos-kernel` on the darwin laptop and fetched a
    # nixos-layer source (found with `TRACE_INPUTS=1`). `optionalAttrs` leaves
    # its argument unevaluated when the condition is false, so on darwin the
    # input is never touched.
    overlays.cachyosKernels =
      final: prev:
      lib.optionalAttrs prev.stdenv.hostPlatform.isLinux (
        inputs.nix-cachyos-kernel.overlays.pinned final prev
      );
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

            kernel = pkgs.cachyosKernels.linux-cachyos-latest.override (super: {
              lto = "thin";
              cpusched = "bore";
              # boot.kernelPatches is merged in automatically by NixOS's core
              # `boot.kernelPackages` option (nixos/modules/system/boot/kernel.nix)
              # -- appending it here too double-applies it and builds a second,
              # redundant kernel derivation.
              stdenv = stdenv;
              extraMakeFlags = [
                "CC=${stdenv.cc}/bin/clang"
                "HOSTCC=${stdenv.cc}/bin/clang"
              ];
            });
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
