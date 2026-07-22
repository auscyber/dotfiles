# sccache for Rust *Nix builds*, cross-platform (NixOS + darwin) — the sibling of
# aspects/dev/ccache.nix, same shape and conventions.
#
# ccache has a first-class `ccacheStdenv` in nixpkgs; sccache has NOTHING
# (no sccacheStdenv, no hook, no module). So instead of swapping `stdenv`, we
# `overrideAttrs` the named rust packages to set `RUSTC_WRAPPER=sccache` plus the
# sccache env, and manage the sccache SERVER in build hooks. `buildRustPackage`
# honours `RUSTC_WRAPPER` transitively (its cargo-build-hook runs plain
# `cargo build`, and cargo reads `RUSTC_WRAPPER` from the environment).
#
# ## Honest caveats (see the plan; these are real)
#
#   - sccache only caches plain library rlib compiles. `build.rs`, proc-macros,
#     and `bin`/`dylib`/`cdylib` LINK steps are never cached -- for many crates
#     that caps the win. Measure with the `--show-stats` block in the build log.
#   - The local disk backend supports ONE server at a time; parallel Nix builds
#     each spawn their own server against the shared SCCACHE_DIR -- the racing
#     case sccache warns about. `SCCACHE_IGNORE_SERVER_IO_ERROR=1` turns a race
#     into a cache MISS instead of a build failure. A per-build
#     `SCCACHE_SERVER_UDS` avoids port-4226 collisions (matters on darwin, where
#     the sandbox is off and builds share host loopback) but not the disk race.
#   - `SCCACHE_BASEDIRS` (the CCACHE_BASEDIR analogue) needs sccache >= 0.14.0;
#     this repo has 0.16.0, so cross-build hits work despite Nix's per-build dirs.
#   - Darwin: the sandbox is off, so no `extra-sandbox-paths` is needed there, and
#     the server can LINGER after the build (double-fork) -- hence the explicit
#     `--stop-server` in postBuild plus a `trap ... EXIT` in preBuild.
#
# Cache dir + `os`-class discipline are identical to ccache: the shared
# `auscybernix.tmpfiles` shim creates `/var/cache/sccache` `root:nixbld 0770` on
# both platforms, and everything under `os` is an option both module systems have.
{ lib, ... }:
let
  # Cross-platform core: declares the options and wires the cache dir, sandbox
  # exposure, and the per-package sccache overlay. Delivered via `os`.
  sccacheModule =
    {
      pkgs,
      lib,
      config,
      ...
    }:
    let
      cfg = config.auscybernix.nix.sccache;
      sccache = lib.getExe pkgs.sccache;
    in
    {
      options.auscybernix.nix.sccache = {
        enable = lib.mkEnableOption "sccache for rust nix builds";
        cacheDir = lib.mkOption {
          type = lib.types.str;
          description = "sccache directory (shared, exposed to the build sandbox).";
          default = "/var/cache/sccache";
        };
        cacheSize = lib.mkOption {
          type = lib.types.str;
          default = "20G";
          description = "SCCACHE_CACHE_SIZE for the local disk backend.";
        };
        packageNames = lib.mkOption {
          type = lib.types.listOf lib.types.str;
          description = "Nix top-level rust packages to compile through sccache.";
          default = [ ];
          example = [
            "lspmux"
            "jj-mcp-server"
          ];
        };
        owner = lib.mkOption {
          type = lib.types.str;
          default = "root";
          description = "Owner of the sccache directory.";
        };
        group = lib.mkOption {
          type = lib.types.str;
          # nixbld is the sandbox build group on both NixOS and nix-darwin
          # (build-users-group = nixbld), so 0770 root:nixbld is writable by the
          # build users on either platform -- same as ccache.
          default = "nixbld";
          description = "Group owner of the sccache directory.";
        };
        trace = lib.mkOption {
          type = lib.types.bool;
          default = false;
          description = "Trace which packages get wrapped with sccache.";
        };
      };

      config = lib.mkIf cfg.enable {
        # Create + own the cache dir (systemd-tmpfiles on NixOS, activation script
        # on darwin). Same shim, same shape as the `10-ccache` rule.
        auscybernix.tmpfiles.settings."20-sccache".${cfg.cacheDir}.d = {
          mode = "0770";
          user = cfg.owner;
          group = cfg.group;
        };

        # Expose the cache to the build sandbox. On NixOS the bind is read-write
        # (nix has no read-only sandbox-path modifier), which is what we need. On
        # darwin the sandbox is off, so this is a harmless no-op.
        nix.settings.extra-sandbox-paths = [ cfg.cacheDir ];

        # `mkAfter` so this wraps whatever a package IS after other overlays have
        # run (e.g. a from-source crate a prior overlay defines), mirroring ccache.
        nixpkgs.overlays = lib.mkAfter [
          (
            final: prev:
            lib.genAttrs cfg.packageNames (
              pn:
              let
                wrapped = prev.${pn}.overrideAttrs (old: {
                  nativeBuildInputs = (old.nativeBuildInputs or [ ]) ++ [ final.sccache ];

                  # cargo reads these from the environment.
                  RUSTC_WRAPPER = sccache;
                  # sccache refuses to cache incremental compilation. Release
                  # builds already default incremental=false, but be explicit
                  # (and correct for buildType="debug").
                  CARGO_INCREMENTAL = "0";
                  SCCACHE_DIR = cfg.cacheDir;
                  SCCACHE_CACHE_SIZE = cfg.cacheSize;
                  # A racing shared-cache IO error becomes a miss, never a build
                  # failure (see the concurrency caveat in the header).
                  SCCACHE_IGNORE_SERVER_IO_ERROR = "1";

                  preBuild = (old.preBuild or "") + ''
                    # Strip the volatile per-build dir from the compile hash so
                    # objects are reusable across Nix builds (sccache >= 0.14).
                    export SCCACHE_BASEDIRS="$NIX_BUILD_TOP"
                    # Per-build socket: avoids the default :4226 collision when
                    # concurrent darwin builds share host loopback.
                    export SCCACHE_SERVER_UDS="$NIX_BUILD_TOP/sccache.sock"
                    ${sccache} --start-server
                    # Best-effort teardown even if the build fails.
                    trap '${sccache} --stop-server || true' EXIT
                  '';

                  postBuild = (old.postBuild or "") + ''
                    # Hit/miss numbers land in the build log -- the only useful
                    # sccache stats (they are per-server-session, not on disk).
                    ${sccache} --show-stats || true
                    # Explicit teardown; matters on darwin where the daemon can
                    # otherwise linger on the host after the build.
                    ${sccache} --stop-server || true
                  '';
                });
              in
              if cfg.trace then builtins.trace "with sccache: ${pn}" wrapped else wrapped
            )
          )
        ];
      };
    };
in
{
  den.aspects.sccache = {
    # Cross-platform core + enable wherever this aspect is included. Enabling with
    # an empty `packageNames` is cheap: it just declares options and creates the
    # cache dir; nothing is wrapped until a package is named.
    os = {
      imports = [ sccacheModule ];
      auscybernix.nix.sccache.enable = true;
    };
  };
}
