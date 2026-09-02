{
  # Patches are auto-included from ./patches/darwin/*.patch.
  den.aspects.nix.darwin =
    { pkgs, ... }:
    {
      nix.distributedBuilds = true;

      nix.linux-builder = {
        enable = true;
        ephemeral = true;
        # The default `pkgs.darwin.linux-builder` runs the guest under QEMU,
        # which on the cross-arch (host qemuArch != guest qemuArch) path hard
        # codes `-machine virt,gic-version=2,accel=hvf:tcg`
        # (nixos/lib/qemu-common.nix) -- and Apple's Hypervisor.framework only
        # implements GICv3, not GICv2, so the VM fails to boot ("HVF does not
        # support GICv2 emulation") and launchd's `KeepAlive` just keeps
        # relaunching it into the same failure. `linux-builder-vz` sidesteps
        # QEMU entirely and runs the guest on Apple's own
        # Virtualization.framework (`vzvm`) instead -- no GIC emulation
        # question to get wrong. Already wired up for `ephemeral` (it keeps
        # the same `.qcow2` name nix-darwin's ephemeral wipe expects) and for
        # `aarch64-linux`-only (the only guest arch it supports; x86_64-linux
        # goes through Rosetta instead of emulation, moot here since that's
        # commented out below anyway).
        package = pkgs.darwin.linux-builder-vz;
        systems = [
          #      "x86_64-linux"
          "aarch64-linux"
        ];
        #    config.boot.binfmt.emulatedSystems = [ "x86_64-linux" ];
      };
    };
}
