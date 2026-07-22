# ccache under Nix, cross-platform (NixOS + darwin).
#
# The whole implementation lives in the `os` class so it forwards into BOTH
# `nixos` and `darwin` (den's os-class battery). For that to eval on both, every
# option the `os` body touches must exist on both module systems:
#
#   - `auscybernix.nix.ccache.*` .. declared HERE (so it exists wherever forwarded)
#   - `auscybernix.tmpfiles.settings` ...... the cross-platform tmpfiles shim
#     (extraModules/{nixos,darwin}/tmpfiles), which replaces the NixOS-only
#     `systemd.tmpfiles.rules` this module used to reach for
#   - `nix.settings.extra-sandbox-paths` ... exists on nixos AND nix-darwin
#   - `nixpkgs.overlays` .................... exists on both
#
# The one genuinely NixOS-only piece -- the setgid `security.wrappers.nix-ccache`
# stats helper -- stays in a `nixos` sibling body; darwin gets a plain-script
# equivalent (no setgid, so no wrapper needed).
#
# NB: the old NixOS module read `config.programs.ccache.cacheDir` (the upstream
# `programs.ccache` module) in two places while its own `programs.ccache.enable`
# was commented out. On darwin `programs.ccache` does not exist at all, and even
# on NixOS that path silently diverges from `cfg.cacheDir` the moment either is
# overridden. This port reads `cfg.cacheDir` everywhere -- single source of truth.
{
  den,
  lib,
  ...
}:
let
  # Shared cross-platform implementation: declares the option set and wires up
  # the cache dir (via tmpfiles), the sandbox exposure, and the ccache overlays.
  # Delivered through `os`, so it is evaluated once per target host class with
  # that class's own `pkgs`/`config`.
  ccacheModule =
    {
      pkgs,
      lib,
      config,
      ...
    }:
    let
      cfg = config.auscybernix.nix.ccache;
      envVars = lib.mapAttrs' (key: value: {
        name = "CCACHE_${key}";
        value = builtins.toString value;
      }) cfg.env;
    in
    {
      options.auscybernix.nix.ccache = {
        enable = lib.mkEnableOption "ccache";
        cacheDir = lib.mkOption {
          type = lib.types.str;
          description = "CCache directory";
          default = "/var/cache/ccache";
        };
        packageNames = lib.mkOption {
          type = lib.types.listOf lib.types.str;
          description = "Nix top-level packages to be compiled using CCache";
          default = [ ];
          example = [
            "wxwidgets_3_2"
            "ffmpeg"
            "libav_all"
          ];
        };
        env = lib.mkOption {
          type = lib.types.attrsOf lib.types.anything;
          default = {
            COMPRESS = 1;
            MAX_SIZE = "10G";
            MAXSIZE = "10G";
          };
          description = "Env";
        };
        owner = lib.mkOption {
          type = lib.types.str;
          default = "root";
          description = "Owner of CCache directory";
        };
        group = lib.mkOption {
          type = lib.types.str;
          # `nixbld` is the sandbox build group on both NixOS and nix-darwin
          # (build-users-group = nixbld), so a 0770 root:nixbld dir is writable
          # by the build users on either platform.
          default = "nixbld";
          description = "Group owner of CCache directory";
        };
        trace = lib.mkOption {
          type = lib.types.bool;
          default = false;
          description = "Trace ccache usage to see which derivations use ccache";
        };
      };

      config = lib.mkIf cfg.enable {
        # Create + own the cache dir. On NixOS this becomes a systemd-tmpfiles
        # rule; on darwin it is compiled into an activation script (mkdir/chmod/
        # chown). Same declaration, both platforms.
        auscybernix.tmpfiles.settings."10-ccache".${cfg.cacheDir}.d = {
          mode = "0770";
          user = cfg.owner;
          group = cfg.group;
        };

        # Expose the cache to the build sandbox. Valid nix.conf setting on both
        # platforms; a no-op where `sandbox = false` (the nix-darwin default),
        # correct once the sandbox is turned on.
        nix.settings.extra-sandbox-paths = [ cfg.cacheDir ];

        # `mkAfter` so these run LAST, after overlays that *define* the named
        # packages. e.g. the lix module adds a from-source `pkgs.lix`; we want to
        # wrap that one, not a stock package a later overlay would then clobber.
        nixpkgs.overlays = lib.mkAfter [
          # Per-package opt-in: rebuild the named top-level packages through
          # ccacheStdenv. Platform-neutral -- ccacheStdenv wraps whatever
          # `stdenv.cc` the platform uses (clang on darwin, gcc on NixOS).
          (
            self: super:
            lib.genAttrs cfg.packageNames (
              pn:
              super.${pn}.override (overrideBefore: {
                stdenv =
                  let
                    newStdenv = self.ccacheStdenv.override { inherit (overrideBefore) stdenv; };
                  in
                  if cfg.trace then builtins.trace "with ccache: ${pn}" newStdenv else newStdenv;
              })
            )
          )
          # Configure the wrapper every ccacheStdenv builds through -- this is
          # what actually points ccache at the shared cache and applies the
          # reproducibility work-arounds (SLOPPINESS/random_seed) that make
          # ccache safe under Nix.
          (self: super: {
            ccacheWrapper = super.ccacheWrapper.override {
              extraConfig = ''
                export CCACHE_COMPRESS=1
                export CCACHE_DIR="${cfg.cacheDir}"
                ${lib.concatStringsSep "\n" (lib.mapAttrsToList (k: v: "export ${k}=${v}") envVars)}
                export CCACHE_BASEDIR="$NIX_BUILD_TOP"          # make in-build absolute paths relative
                # Nix builds are already hermetic (pinned store paths), so we can
                # be aggressive here without risking a wrong cache hit:
                #   pch_defines    - REQUIRED to cache precompiled-header compiles;
                #                    without it EVERY PCH-using TU misses. Lix uses
                #                    PCH (meson enable-pch-std), which is why it
                #                    barely cached before this.
                #   file_macro     - don't hash __FILE__, so a build in a different
                #                    /private/tmp/nix-build-*.drv-N dir still hits
                #                    (belt-and-suspenders with CCACHE_BASEDIR).
                #   system_headers - don't hash system-header contents (pinned in
                #                    the store anyway); fewer spurious misses.
                #   locale         - ignore LANG/LC_* in the hash.
                export CCACHE_SLOPPINESS=random_seed,time_macros,include_file_mtime,include_file_ctime,pch_defines,file_macro,system_headers,locale
                export CCACHE_UMASK=007
                if [ ! -d "$CCACHE_DIR" ]; then
                  echo "====="
                  echo "Directory '$CCACHE_DIR' does not exist"
                  echo "Please create it with:"
                  echo "  sudo mkdir -m0770 '$CCACHE_DIR'"
                  echo "  sudo chown ${cfg.owner}:${cfg.group} '$CCACHE_DIR'"
                  echo "====="
                  exit 1
                fi
                if [ ! -w "$CCACHE_DIR" ]; then
                  echo "====="
                  echo "Directory '$CCACHE_DIR' is not accessible for user $(whoami)"
                  echo "Please verify its access permissions"
                  echo "====="
                  exit 1
                fi
              '';
            };
          })
        ];
      };
    };

  # NixOS-only: the setgid perl helper that lets a normal user read/clear cache
  # stats (`nix-ccache -s`). Needs `security.wrappers`, which nix-darwin lacks.
  ccacheExtras =
    {
      pkgs,
      lib,
      config,
      ...
    }:
    let
      cfg = config.auscybernix.nix.ccache;
      envVars = lib.mapAttrs' (key: value: {
        name = "CCACHE_${key}";
        value = builtins.toString value;
      }) cfg.env;
    in
    lib.mkIf cfg.enable {
      security.wrappers.nix-ccache = {
        inherit (cfg) owner group;
        setuid = false;
        setgid = true;
        source =
          let
            perl = pkgs.perl.withPackages (p: [ p.JSON ]);
          in
          pkgs.writeScript "nix-ccache.pl" ''
            #!${perl}/bin/perl
            use JSON::PP;
            my $json_script = q|${builtins.toJSON envVars}|;
            my $extra_env=decode_json ($json_script);

            %ENV=( CCACHE_DIR => '${cfg.cacheDir}', %{$extra_env} );
            sub untaint {
              my $v = shift;
              return '-C' if $v eq '-C' || $v eq '--clear';
              return '-V' if $v eq '-V' || $v eq '--version';
              return '-s' if $v eq '-s' || $v eq '--show-stats';
              return '-z' if $v eq '-z' || $v eq '--zero-stats';
              exec('${pkgs.ccache}/bin/ccache', '-h');
            }
            exec('${pkgs.ccache}/bin/ccache', map { untaint $_ } @ARGV);
          '';
      };
    };
in
{
  den.aspects.ccache = {
    # The cross-platform core, plus enabling ccache wherever this aspect is
    # included (the kernel aspects and any host that lists it). `imports` pulls
    # in the option-declaring module; the sibling attr sets `enable`.
    os = {
      imports = [
        ccacheModule
        ccacheExtras
      ];
      auscybernix.nix.ccache.enable = true;
    };

    # auspc gets a bigger cache than the 10G default.
    provides.auspc.nixos = {
      auscybernix.nix.ccache.env.MAX_SIZE = "15G";
    };
    provides.Ivys-MacBook-Pro.darwin = {
      auscybernix.nix.ccache.env.MAX_SIZE = "5G";
    };
  };
}
