{
  den,
  lib,
  config,
  inputs,
  ...
}:
let
  nvfetcherSourceSubmodule = lib.types.submodule (_: {
    options = {
      src = lib.mkOption {
        type = lib.types.attrs;
        description = "URL of the nvfetcher source repository.";
      };
      fetch = lib.mkOption {
        type = lib.types.attrs;
        default = null;
        description = "Function to fetch the nvfetcher source code. If not set, the default fetcher will be used.";
      };
      git = lib.mkOption {
        type = lib.types.nullOr (lib.types.attrs);
        default = null;
        description = "Git repository information for nvfetcher. If not set, the default will be used.";
      };
      cargo_lock = lib.mkOption {
        type = lib.types.nullOr (lib.types.listOf lib.types.str);
        default = null;
        description = "Path to the cargo lock file for nvfetcher. If not set, the default will be used.";
      };
      script = lib.mkOption {
        type = lib.types.nullOr (lib.types.functionTo lib.types.lines);
        default = null;
        description = ''
          Optional post-fetch hook, written as `pkgs: <shell script>`.

          Run at update time by `update-sources` (and standalone by
          `postprocess-sources`) — NOT during evaluation — so it may hit the
          network. The fetched source tree is in `$src` and a scratch output
          directory in `$out`; whatever the script leaves in `$out` is merged
          into the source's committed `_sources/sha256-<srcHash>/` folder and
          surfaced to consumers as `sources.<name>.output` and
          `sources.extra.<name>`.

          `pkgs` is the same perSystem package set `update-sources` runs in.
          There is deliberately no separate inputs list: reference tools by full
          store path (e.g. `''${pkgs.cargo}/bin/cargo`) or set up `PATH` yourself
          inside the script.
        '';
      };
    };
  });

  description = ''
    Provides `sources` (the result of `_sources/generated.nix` callPackage'd
    against per-system pkgs) as a top-level module argument.

    nvfetcher emits `_sources/generated.{json,nix}` describing every nvchecker
    source. This battery makes those evaluated sources available as a `sources`
    module-arg to every host, user, and home — so overlays and aspects can
    `{ sources, ... }: ...` without importing the generated file themselves.

    ## Usage

    Globally (recommended; default-included in den.default):

        den.default.includes = [ den.batteries.sources ];

    Specific:

        den.aspects.my-laptop.includes = [ den.batteries.sources ];

    ## Behaviour

    Contextual — when included in a host aspect, supplies `sources` for the host's
    OS class; when included in a user/home aspect, supplies `sources` for the
    corresponding Home Manager configuration.

    ## Post-fetch scripts

    A source may carry a `script` (`pkgs: <shell>`). It runs at update time with
    the fetched tree in `$src` and a scratch dir in `$out`; the `$out` contents
    are merged into the source's `_sources/sha256-<srcHash>/` folder and
    re-exposed as `sources.<name>.output` and under `sources.extra.<name>`.
  '';

  # Non-nvfetcher option keys that must never reach the generated TOML.
  nonTomlKeys = [ "script" ];

  # Sources that declare a post-fetch `script`.
  scriptedSources = lib.filterAttrs (_: v: v.script != null) config.nvfetcher.sources;

  # Augment a `sources` set with each source's on-disk artifact folder
  # `_sources/sha256-<srcHash>/` — where nvfetcher stores cargo_lock/extract
  # files and where post-fetch `script`s bake their output. Hashes come from
  # `generated.json` (a pure file read); we deliberately do NOT read
  # `srcs.<name>.src.outputHash`, because forcing `.src` realises
  # builtins.fetchTarball sources over the network at *eval* time. Folders are
  # checked against disk, so `.output`/`.extra` reflect what actually exists.
  # Verbatim twin of the helper in aspects/utils/overlays.nix.
  withExtra =
    srcs:
    let
      meta = builtins.fromJSON (builtins.readFile ../../_sources/generated.json);
      sanitize = builtins.replaceStrings [ "/" ] [ "_" ];
      folderOf =
        name:
        let
          h = meta.${name}.src.sha256 or null;
        in
        if h == null then null else ../../_sources + "/${sanitize h}";
      existing = lib.filterAttrs (_: p: p != null && builtins.pathExists p) (
        lib.mapAttrs (name: _: folderOf name) srcs
      );
    in
    srcs
    // {
      extra = existing;
    }
    // lib.mapAttrs (name: p: srcs.${name} // { output = p; }) existing;

  # Build `sources` from a bare `inputs.nixpkgs` rather than `withSystem system`
  # (the final perSystem pkgs). nvfetcher sources are plain fetcher derivations —
  # overlays never affect them — and `withSystem` creates a host -> perSystem
  # back-edge that infinite-loops when a host is resolved *inside* the
  # flake-parts/perSystem scope (e.g. by `flake-parts-to-host` overlay collection).
  mkAspect = class: system: {
    ${class}._module.args.sources = withExtra (
      (import inputs.nixpkgs { inherit system; }).callPackage ../../_sources/generated.nix { }
    );
  };

  osAspect =
    { host }:
    {
      name = "sources/os";
    }
    // lib.optionalAttrs (host ? class) (mkAspect host.class host.system);

  userAspect =
    {
      user,
      host,
    }:
    {
      name = "sources/user";
      includes = map (c: mkAspect c host.system) user.classes;
    };

  hmAspect =
    { home }:
    {
      name = "sources/home";
    }
    // lib.optionalAttrs (home ? class) (mkAspect home.class home.system);
in
{
  options.nvfetcher = {
    sources = lib.mkOption {
      type = lib.types.attrsOf nvfetcherSourceSubmodule;
      default = { };
    };
  };

  config = {
    flake.lib.withExtra = withExtra;
    ff.nvfetcher.url = "github:berberman/nvfetcher";
    patchedInputs.nvfetcher = {
      patches = [ ../../patches/nvfetcher/nvfetcher.patch ];
    };

    den.batteries.sources = {
      name = "sources";
      inherit description;
      includes = [
        osAspect
        userAspect
        hmAspect
      ];
    };

    den.default.includes = [ den.batteries.sources ];

    perSystem =
      args@{
        pkgs,
        inputs',
        ...
      }:
      let
        # Strip our non-nvfetcher option keys before serialising the TOML.
        tomlSources = lib.mapAttrs (_: v: removeAttrs v nonTomlKeys) config.nvfetcher.sources;
        nvFetcherConfig = lib.attrsets.filterAttrsRecursive (_: v: v != null) tomlSources;
        configFile = pkgs.writers.writeTOML "nvfetcher.toml" nvFetcherConfig;

        # Raw (un-augmented) sources — used to realise each `src` for its script.
        rawSources = pkgs.callPackage ../../_sources/generated.nix { };

        # One shell fragment per scripted source: realise `$src`, run the user
        # script against a scratch `$out`, and merge `$out` into the source's
        # `_sources/sha256-<srcHash>/` folder (same place nvfetcher stores its own
        # extract/cargo_lock files — so it is NOT wiped). Run relative to the repo
        # root (cwd), matching how nvfetcher itself writes `_sources/generated.*`.
        mkSnippet =
          name: cfg:
          let
            folder = builtins.replaceStrings [ "/" ] [ "_" ] rawSources.${name}.src.outputHash;
          in
          ''
            echo ">> postprocess: ${name}"
            src="${rawSources.${name}.src}"
            export src
            out="$(mktemp -d)"
            export out
            (
            ${cfg.script pkgs}
            )
            dest="_sources/${folder}"
            mkdir -p "$dest"
            cp -RL "$out"/. "$dest"/
            chmod -R u+w "$dest"
          '';

        postprocessSources =
          (pkgs.writeShellApplication {
            name = "postprocess-sources";
            # Only the wrapper's own coreutils (mktemp/mkdir/cp/chmod). Script tool
            # deps are the script's business — reference them by full store path.
            runtimeInputs = [ pkgs.coreutils ];
            text = ''
              set -eu
              if [ ! -e flake.nix ]; then
              	echo "postprocess-sources: run from the repo root (flake.nix not found)" >&2
              	exit 1
              fi
              ${lib.concatStringsSep "\n" (lib.mapAttrsToList mkSnippet scriptedSources)}
              echo "Post-processed sources written to _sources/."
            '';
          }).overrideAttrs
            (_: {
              # `script`s are arbitrary user shell embedded verbatim; don't fail the
              # build on shellcheck lint (e.g. SC2155) or `bash -n` over them.
              checkPhase = ":";
            });

        updateSources = pkgs.writeShellApplication {
          name = "update-sources";
          text = ''
            set -eu
            env -u GIT_DIR
            # nvchecker key file: matches the agenix-rendered template the
            # home-base aspect writes to $HOME/.config/nvchecker.toml.
            # Override with NVCHECKER_KEYS=/path or `--keyfile <path>`.
            KEY_FILE="''${NVCHECKER_KEYS:-$HOME/.config/nvchecker.toml}"
            while [ "$#" -gt 0 ]; do
            	case "$1" in
            	--keyfile | -k)
            		KEY_FILE="$2"
            		shift 2
            		;;
            	*)
            		shift
            		;;
            	esac
            done

            echo "Updating nvfetcher source code..."
            if [ -r "$KEY_FILE" ]; then
            	echo "Using key file: $KEY_FILE"
            	${lib.getExe inputs'.nvfetcher.packages.default} -c ${configFile} -k "$KEY_FILE"
            else
            	echo "Note: $KEY_FILE not readable; running without -k (github API may rate-limit)" >&2
            	${lib.getExe inputs'.nvfetcher.packages.default} -c ${configFile}
            fi
            echo "Source code updated successfully."

            echo "Post-processing sources..."
            ${lib.getExe postprocessSources}
          '';
        };
      in
      {
        devshells.default.packages = [
          updateSources
          postprocessSources
        ];
        _module.args.sources = withExtra rawSources;

        packages.update-sources = updateSources;
        packages.postprocess-sources = postprocessSources;

        apps.update-sources = {
          type = "app";
          program = lib.getExe updateSources;
        };
        apps.postprocess-sources = {
          type = "app";
          program = lib.getExe postprocessSources;
        };
      };
  };
}
