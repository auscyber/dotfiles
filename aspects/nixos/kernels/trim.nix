{
  lib,
  ...
}:
let
  # Identifies our own entry in `boot.kernelPatches` so the generator can strip
  # it back off again (see `kernelTrimBase` below).
  patchName = "localmodconfig-trim";
in
{
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
  # The generated JSON is applied as a `boot.kernelPatches` entry rather than as
  # a replacement kernel built from a frozen .config. That matters:
  #
  #   * nixpkgs still assembles the config — common-config.nix, the host's other
  #     kernelPatches, everything NixOS requires of a kernel — and this only
  #     subtracts from the result. A frozen .config bypasses all of it and
  #     silently drops whatever NixOS added since the file was generated.
  #   * it composes with whatever `boot.kernelPackages` already is (cachyos on
  #     auspc, zen, vanilla) instead of pinning the host to linuxPackages_latest.
  #   * unknown symbols after a kernel bump are just dropped by kconfig, instead
  #     of the whole config silently reverting to the old kernel's defaults.
  den.aspects.kernel-trim = {
    nixos =
      {
        config,
        lib,
        host,
        ...
      }:
      let
        cfg = config.boot.kernelTrim;

        # The host's real kernel with our trim removed again. Everything the
        # generator measures hangs off this, so localmodconfig always runs
        # against the untrimmed config instead of feeding its own output back
        # in and converging on whatever the first run happened to produce.
        baseKernel = config.boot.kernelPackages.kernel.override (
          orig: {
            kernelPatches = builtins.filter (p: (p.name or "") != patchName) (orig.kernelPatches or [ ]);
          }
        );

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

        trim =
          lib.importJSON cfg.file
          |> lib.filterAttrs (
            name: value: !(lib.elem name cfg.ignore) && (!cfg.onlyDisable || value == "n")
          )
          |> lib.mapAttrs (_: value: lib.mkOverride cfg.priority (toKernelItem value));
      in
      {
        options.boot.kernelTrim = {
          enable = lib.mkEnableOption "the generated localmodconfig kernel trim" // {
            default = builtins.pathExists cfg.file;
            defaultText = lib.literalMD "`true` once {option}`boot.kernelTrim.file` exists";
          };

          file = lib.mkOption {
            type = lib.types.path;
            default = ../../hosts + "/${host.name}/kernel-trim.json";
            defaultText = lib.literalExpression ''aspects/hosts/''${host.name}/kernel-trim.json'';
            description = ''
              Symbol -> value map produced by `nix run .#gen-kernel-trim`, as a
              diff against this host's untrimmed kernel config. Values are the
              raw `.config` right-hand sides: `"y"`, `"m"`, `"n"`, or a freeform
              string.
            '';
          };

          onlyDisable = lib.mkOption {
            type = lib.types.bool;
            default = true;
            description = ''
              Keep only the entries that turn something off. localmodconfig's
              whole job is disabling unused modules, so anything it *enables* is
              olddefconfig fallout rather than a measurement of this machine —
              and enabling a symbol over the top of nixpkgs' own config is the
              half that can break a boot. Turn this off to apply the full diff.
            '';
          };

          ignore = lib.mkOption {
            type = lib.types.listOf lib.types.str;
            default = [ ];
            example = [ "USB_XHCI_HCD" ];
            description = ''
              Config symbols (without the `CONFIG_` prefix) to drop from the
              generated trim — the escape hatch for a driver modprobed.db never
              saw because the hardware was unplugged when it was recorded.
            '';
          };

          priority = lib.mkOption {
            type = lib.types.int;
            default = 60;
            description = ''
              Override priority for the generated entries. The default sits
              between a normal definition (100) and `lib.mkForce` (50), so the
              trim wins over nixpkgs' defaults while a hand-written
              `structuredExtraConfig` using `mkForce` still wins over the trim.
            '';
          };
        };

        config = {
          # The builder `scripts/gen-kernel-trim.sh` drives: takes a file of
          # module names and emits a directory with the config before and after
          # `make localmodconfig`, both from one source tree so the diff cannot
          # straddle two kernel versions.
          system.build.kernelTrimBase = baseKernel;
          system.build.kernelLocalmodconfig =
            lsmod:
            baseKernel.configfile.overrideAttrs (old: {
              pname = "linux-config-localmodconfig";
              buildPhase = old.buildPhase + ''
                cp "$buildRoot/.config" "$buildRoot/.config.base"

                echo "running localmodconfig against ${lsmod}..."
                # localmodconfig ends in an interactive `oldconfig`; feed it
                # blank lines so every new symbol takes its default. `yes` dies
                # of SIGPIPE when make exits, which pipefail would otherwise
                # treat as a failure.
                { yes "" || true; } | make $makeFlags -C . O="$buildRoot" LSMOD=${lsmod} localmodconfig
              '';
              installPhase = ''
                mkdir -p "$out"
                mv "$buildRoot/.config.base" "$out/base.config"
                mv "$buildRoot/.config" "$out/trimmed.config"
              '';
            });

          boot.kernelPatches = lib.mkIf cfg.enable [
            {
              name = patchName;
              patch = null;
              structuredExtraConfig = trim;
            }
          ];
        };
      };
  };
}
