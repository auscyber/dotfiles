# `nix run .#build-without-celler` -- build with the celler cache taken out of
# the substituter list, and nothing else changed.
#
# Worth having because cache.ivymect.in is a cache this flake both *writes* and
# *trusts*, which makes it the one substituter that can be wrong in a way
# upstream caches cannot: a half-pushed NAR, a stale signature after
# celler-keys.json drifts, or the box simply being down and every fetch hanging
# on a connect timeout. Pulling it out is the difference between "my build is
# broken" and "my cache is broken", and there is otherwise no quick way to ask.
#
#   nix run .#build-without-celler                 # this host's system closure
#   nix run .#build-without-celler -- .#foo        # any installable
#   nix run .#build-without-celler -- --rebuild .#foo
#
# The agenix wrappers (aspects/security/agenix-rekey.nix) apply the same filter
# unconditionally, since a dead cache breaking `secret-edit` is never useful.
# Both share lib/without-celler.nix so the vhost is named once.
#
# It does NOT drop the celler public keys from trusted-public-keys: a path
# already in the local store, or one another substituter happens to serve, stays
# usable. The point is to stop *fetching* from celler, not to distrust what it
# signed.
{ lib, ... }:
let
  withoutCeller = import ../../lib/without-celler.nix { inherit lib; };
in
{
  perSystem =
    { pkgs, config, ... }:
    {
      # Package as well as app: writeShellApplication runs shellcheck in its
      # builder, and an `apps.<x>.program` string cannot be built directly, so
      # app-only scripts are never linted. `nix build .#build-without-celler` does.
      packages.build-without-celler = pkgs.writeShellApplication {
        name = "build-without-celler";
        runtimeInputs = withoutCeller.deps pkgs;
        text = ''
          ${withoutCeller.snippet}

          if [ -z "$celler_free_substituters" ]; then
            echo "build-without-celler: nothing to do -- DENDRITIC_USE_CELLER=1 is set" >&2
            exit 1
          fi

          # Default to this machine's own system closure, which is the
          # thing you actually want rebuilt when you suspect the cache.
          args=("$@")
          if [ ''${#args[@]} -eq 0 ]; then
            host="$(uname -n)"
            host="''${host%%.*}"
            case "$(uname -s)" in
              Darwin) attr=".#darwinConfigurations.$host.system" ;;
              *)      attr=".#nixosConfigurations.$host.config.system.build.toplevel" ;;
            esac
            echo "build-without-celler: no args, building $attr" >&2
            args=("$attr")
          fi

          # `nix build` unless the first argument is itself a subcommand,
          # so `-- path .#foo` or `-- develop` work too.
          case "''${args[0]}" in
            build|path|develop|shell|run|copy|why-depends|eval) cmd=("''${args[@]}") ;;
            *) cmd=(build "''${args[@]}") ;;
          esac

          echo "build-without-celler: substituters = $celler_free_substituters" >&2

          # --substituters as well as the NIX_CONFIG the snippet exported:
          # the flag is what this invocation reads, NIX_CONFIG is what any
          # nested nix inherits. --no-accept-flake-config matters just as
          # much, since the flake's own nixConfig can name celler and would
          # otherwise offer to add it straight back.
          exec nix "''${cmd[@]}" \
            --no-accept-flake-config \
            --substituters "$celler_free_substituters"
        '';
      };

      apps.build-without-celler = {
        type = "app";
        program = lib.getExe config.packages.build-without-celler;
      };
    };
}
