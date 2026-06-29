{
  den,
  lib,
  config,
  withSystem,
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
      cargo_lock = lib.mkOption {
        type = lib.types.nullOr (lib.types.listOf lib.types.str);
        default = null;
        description = "Path to the cargo lock file for nvfetcher. If not set, the default will be used.";
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
  '';

  mkAspect =
    class: system:
    withSystem system (
      { pkgs, ... }:
      {
        ${class}._module.args.sources = pkgs.callPackage ../../_sources/generated.nix { };
      }
    );

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
        nvFetcherConfig = lib.attrsets.filterAttrsRecursive (_: v: v != null) config.nvfetcher.sources;
        configFile = pkgs.writers.writeTOML "nvfetcher.toml" nvFetcherConfig;
      in
      {
        devshells.default.packages = [ args.config.packages.update-sources ];
        _module.args.sources = pkgs.callPackage ../../_sources/generated.nix { };

        packages.update-sources = pkgs.writeShellApplication {
          name = "update-sources";
          text = ''
            set -eu
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
          '';
        };
      };
  };
}
