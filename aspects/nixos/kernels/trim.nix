{
  lib,
  ...
}:
let
  # Identifies our own entry in a kernel's `kernelPatches` so `untrimmed` can
  # strip it back off again before measuring.
  patchName = "localmodconfig-trim";

  toKernelItem =
    value:
    if value == "y" then
      lib.kernel.yes
    else if value == "m" then
      lib.kernel.module
    else if value == "n" then
      lib.kernel.no
    else
      lib.kernel.freeform value;

  # A generated trim, as a plain `kernelPatches` entry. Nothing NixOS-specific
  # about it: hand it to `boot.kernelPatches`, to a `buildLinux`/`.override`
  # call, or to anything else that takes kernel patches.
  #
  #   kernelPatches = [ (self.lib.kernelTrim.patch { file = ./kernel-trim.json; }) ];
  #
  # The generated JSON is applied this way rather than as a replacement kernel
  # built from a frozen .config. That matters:
  #
  #   * nixpkgs still assembles the config — common-config.nix, the kernel's
  #     other kernelPatches, everything NixOS requires of a kernel — and this
  #     only subtracts from the result. A frozen .config bypasses all of it and
  #     silently drops whatever NixOS added since the file was generated.
  #   * it composes with whatever kernel it is attached to (cachyos on auspc,
  #     zen, vanilla, a locally overridden one) instead of pinning anything.
  #   * unknown symbols after a kernel bump are just dropped by kconfig, instead
  #     of the whole config silently reverting to the old kernel's defaults.
  patch =
    {
      file,
      ignore ? [ ],
      onlyDisable ? true,
      priority ? 60,
    }:
    {
      name = patchName;
      patch = null;
      structuredExtraConfig =
        lib.importJSON file
        |> lib.filterAttrs (name: value: !(lib.elem name ignore) && (!onlyDisable || value == "n"))
        |> lib.mapAttrs (_: value: lib.mkOverride priority (toKernelItem value));
    };

  # A kernel with our trim removed again. Everything the generator measures
  # hangs off this, so localmodconfig always runs against the untrimmed config
  # instead of feeding its own output back in and converging on whatever the
  # first run happened to produce.
  untrimmed =
    kernel:
    kernel.override (orig: {
      kernelPatches = builtins.filter (p: (p.name or "") != patchName) (orig.kernelPatches or [ ]);
    });

  # The builder `scripts/gen-kernel-trim.sh` drives: takes any kernel and a file
  # of module names, and emits a directory with that kernel's config before and
  # after `make localmodconfig`, both from one source tree so the diff cannot
  # straddle two kernel versions.
  localmodconfig =
    { kernel, lsmod }:
    (untrimmed kernel).configfile.overrideAttrs (old: {
      pname = "linux-config-localmodconfig";
      buildPhase = old.buildPhase + ''
        cp "$buildRoot/.config" "$buildRoot/.config.base"

        echo "running localmodconfig against ${lsmod}..."
        # localmodconfig ends in an interactive `oldconfig`; feed it blank lines
        # so every new symbol takes its default. `yes` dies of SIGPIPE when make
        # exits, which pipefail would otherwise treat as a failure.
        { yes "" || true; } | make $makeFlags -C . O="$buildRoot" \
          ARCH=$kernelArch LSMOD=${lsmod} localmodconfig
      '';
      installPhase = ''
        mkdir -p "$out"
        mv "$buildRoot/.config.base" "$out/base.config"
        mv "$buildRoot/.config" "$out/trimmed.config"

        # Every symbol that something else `select`s, straight out of this
        # kernel's own Kconfig. kconfig will not let a selected symbol drop
        # below its selector's value: it re-prompts ("(SYM) [Y/?]"), and
        # nixpkgs' generate-config.pl answers the trim's "n" a second time and
        # dies with "repeated question". The generator drops these from the
        # diff — localmodconfig's output is only self-consistent as a whole
        # config, not as independent answers replayed through `make config`.
        grep -rhE '^[[:space:]]*select[[:space:]]+[A-Za-z0-9_]+' --include='Kconfig*' . \
          | awk '{ print $2 }' | sort -u > "$out/selected.syms"
      '';
    });
in
{
  # Kernel-level, host-independent: usable against any kernel derivation, not
  # just the one a host's `boot.kernelPackages` resolves to.
  flake.lib.kernelTrim = {
    inherit
      patchName
      patch
      untrimmed
      localmodconfig
      ;
  };

  perSystem =
    { pkgs, ... }:
    {
      # Regenerates aspects/hosts/<host>/kernel-trim.json. Must run ON the host
      # being trimmed — it reads that machine's modprobed.db.
      apps.gen-kernel-trim = {
        type = "app";
        program = lib.getExe (
          pkgs.writeShellApplication {
            name = "gen-kernel-trim";
            runtimeInputs = with pkgs; [
              nix
              jq
              gawk
              coreutils
            ];
            text = builtins.readFile ../../../scripts/gen-kernel-trim.sh;
          }
        );
      };
    };

  # Shrink the kernel down to the drivers this machine actually loads, by
  # feeding `make localmodconfig` a module list built from modprobed.db plus the
  # (facter-derived) initrd module lists.
  #
  # Including the aspect picks up aspects/hosts/<host>/kernel-trim.json and adds
  # it to whatever kernel that host already uses — nothing else. Anything to
  # tune (`ignore`, `onlyDisable`, `priority`) is an argument to `patch`, so a
  # host that needs it skips the aspect and writes the entry itself:
  #
  #   boot.kernelPatches = [
  #     (self.lib.kernelTrim.patch {
  #       file = ./auspc/kernel-trim.json;
  #       ignore = [ "USB_XHCI_HCD" ];
  #     })
  #   ];
  den.aspects.kernel-trim.nixos =
    { host, ... }:
    let
      file = ../../hosts + "/${host.name}/kernel-trim.json";
    in
    {
      # Inert until `nix run .#gen-kernel-trim`, run on that host, writes it.
      boot.kernelPatches = lib.optional (builtins.pathExists file) (patch { inherit file; });
    };
}
