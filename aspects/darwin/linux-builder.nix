{ den, ... }:
{
  # Its OWN aspect, not part of `den.aspects.nix`.
  #
  # nix-darwin's `nix.linux-builder.package` carries
  # `apply = pkg: pkg.override { modules = old.modules ++ [ cfg.config ]; }`,
  # so the guest NixOS system is re-evaluated inside every darwin fixpoint --
  # unconditionally, even with an empty `config`, and regardless of whether the
  # package is precomputed, because nix memoises thunks rather than function
  # applications. On this laptop that is four evaluations of a full NixOS system
  # (base plus three specialisations), and it is where the bulk of a host eval
  # goes: `pam.nix` alone reaches 1667 `evalModules` per guest.
  #
  # Welded into `den.aspects.nix` there was no way to opt out, because nothing
  # can exclude the `nix` aspect. As its own aspect a specialisation can drop it
  #   den.aspects.<host>.specialisations.study.excludes = [ den.aspects.linux-builder ];
  # which is worth doing for any profile that has no reason to build Linux.
  den.aspects.nix.includes = [ den.aspects.linux-builder ];

  # Patches are auto-included from ./patches/darwin/*.patch.
  den.aspects.linux-builder.darwin = { pkgs, ... }: {
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
        "x86_64-linux"
        "aarch64-linux"
      ];
      config.boot.binfmt.emulatedSystems = [ "x86_64-linux" ];

      # A builder VM has no reader, and NixOS's documentation machinery is not
      # cheap: `make-options-doc` re-evaluates the whole module set to render
      # option docs, which showed up as ~2000 `evalModules` inside a plain
      # `darwinConfigurations.<host>.system.drvPath` (measured with
      # `--trace-function-calls`). Nothing in the guest serves man pages or the
      # manual, so none of it is reachable at runtime either.
      config.documentation = {
        enable = false;
        nixos.enable = false;
        man.enable = false;
        info.enable = false;
        doc.enable = false;
      };
    };
  };
}
