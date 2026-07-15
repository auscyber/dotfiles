{
  inputs,
  lib,
  ...
}:
# Lightweight, eval-driven flake checks that assert on key host features
# without forcing a full system build (no stylix IFD, no remote builders).
# Run with `nix flake check` on an x86_64-linux machine/builder.
{
  perSystem =
    {
      pkgs,
      system,
      ...
    }:
    lib.optionalAttrs (system == "x86_64-linux") {
      checks =
        let
          # Build a trivial derivation that succeeds iff every condition in
          # `conds` (a name -> bool attrset) holds. Failing names are printed.
          assertCheck =
            name: conds:
            let
              failed = lib.attrNames (lib.filterAttrs (_: ok: !ok) conds);
            in
            pkgs.runCommand "check-${name}" { } (
              if failed == [ ] then
                "echo ok > $out"
              else
                "echo 'FAILED assertions: ${lib.concatStringsSep ", " failed}' >&2; exit 1"
            );

          secondpc = inputs.self.nixosConfigurations.secondpc.config;
          auspc = inputs.self.nixosConfigurations.auspc.config;
        in
        {
          # disko generates the expected on-disk layout.
          secondpc-filesystems = assertCheck "secondpc-filesystems" {
            "/ is zfs zroot/nixos" =
              secondpc.fileSystems."/".fsType == "zfs" && secondpc.fileSystems."/".device == "zroot/nixos";
            "/mnt/hdd is zfs zpool/root" =
              secondpc.fileSystems."/mnt/hdd".fsType == "zfs"
              && secondpc.fileSystems."/mnt/hdd".device == "zpool/root";
            "/boot/efi is vfat" = secondpc.fileSystems."/boot/efi".fsType == "vfat";
          };

          # base disko actually saw both pools.
          secondpc-disko-pools = assertCheck "secondpc-disko-pools" {
            "zroot pool defined" = secondpc.disko.devices.zpool ? zroot;
            "zpool pool defined" = secondpc.disko.devices.zpool ? zpool;
          };

          # facter report is wired and populating hardware detection.
          secondpc-facter = assertCheck "secondpc-facter" {
            "initrd modules detected from report" = secondpc.boot.initrd.availableKernelModules != [ ];
            "ahci present (SATA controller from report)" =
              builtins.elem "ahci" secondpc.boot.initrd.availableKernelModules;
          };

          # Host identity / zfs support survived the conversion.
          secondpc-identity = assertCheck "secondpc-identity" {
            "hostId preserved" = secondpc.networking.hostId == "4f6f802e";
            "zfs supported" =
              let
                sf = secondpc.boot.supportedFilesystems;
              in
              if lib.isList sf then builtins.elem "zfs" sf else (sf.zfs or false);
          };

          # auspc: disko generates the zfs root pool mounts; /boot + /mnt/hdd
          # stay inline (by-uuid) to avoid by-partlabel churn on the live box.
          auspc-filesystems = assertCheck "auspc-filesystems" {
            "/ is zfs zpool/root" =
              auspc.fileSystems."/".fsType == "zfs" && auspc.fileSystems."/".device == "zpool/root";
            "/nix is zfs zpool/nix" =
              auspc.fileSystems."/nix".fsType == "zfs" && auspc.fileSystems."/nix".device == "zpool/nix";
            "/var is zfs zpool/var" =
              auspc.fileSystems."/var".fsType == "zfs" && auspc.fileSystems."/var".device == "zpool/var";
            "/home is zfs zpool/home" =
              auspc.fileSystems."/home".fsType == "zfs" && auspc.fileSystems."/home".device == "zpool/home";
            "/boot is vfat" = auspc.fileSystems."/boot".fsType == "vfat";
            "/mnt/hdd is ext4" = auspc.fileSystems."/mnt/hdd".fsType == "ext4";
          };

          # base disko saw the auspc root pool.
          auspc-disko-pools = assertCheck "auspc-disko-pools" {
            "zpool pool defined" = auspc.disko.devices.zpool ? zpool;
          };

          # facter report is wired and populating hardware detection.
          auspc-facter = assertCheck "auspc-facter" {
            "initrd modules detected from report" = auspc.boot.initrd.availableKernelModules != [ ];
            "nvme present (NVMe controller from report)" =
              builtins.elem "nvme" auspc.boot.initrd.availableKernelModules;
          };

          auspc-identity = assertCheck "auspc-identity" {
            "hostId preserved" = auspc.networking.hostId == "230c61e9";
            "zfs supported" =
              let
                sf = auspc.boot.supportedFilesystems;
              in
              if lib.isList sf then builtins.elem "zfs" sf else (sf.zfs or false);
          };
        };
    };
}
