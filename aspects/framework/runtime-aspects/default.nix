{
  den,
  lib,
  config,
  self,
  ...
}:
# Runtime aspects, after tupakkatapa/nixos-runtime-modules but by DIFF: a
# registered aspect is excluded from its host's base. `aspect build <name>`
# evaluates the base plus that aspect (on demand, never at switch time) and
# ./runtime.py keeps only the difference; `aspect up/down` then links or
# unlinks those files and launchd jobs, merging line insertions into shared
# files, with no evaluation at all. Base activation (boot included) undoes it.
let
  inherit (lib) mkOption types;

  hostsOf = lib.concatMapAttrs (_: hs: hs) den.hosts;

  entryType = types.submodule {
    options = {
      desc = mkOption {
        type = types.str;
        default = "";
      };
      postEnable = mkOption {
        type = types.nullOr types.str;
        default = null;
      };
      postDisable = mkOption {
        type = types.nullOr types.str;
        default = null;
      };
    };
  };

  hostType = types.submodule {
    options = {
      aspects = mkOption {
        type = types.attrsOf entryType;
        default = { };
        description = "Aspects this host loads only on request, keyed by aspect name.";
      };
      # Only DECLARED `requires`: derived ones come from capability resolution,
      # which already sees these excludes.
      fromRequires = mkOption {
        type = types.bool;
        default = false;
        description = "Also register every aspect that declares `requires`.";
      };
      enabled = mkOption {
        type = types.listOf types.str;
        default = [ ];
        internal = true;
        description = "Set only in the variant `aspect build` evaluates.";
      };
    };
  };

  registryOf =
    hostName:
    let
      r = config.den.runtime.${hostName} or null;
    in
    if r == null then
      { }
    else
      lib.optionalAttrs r.fromRequires (
        lib.mapAttrs (_: _: {
          desc = "gated by its requires";
          postEnable = null;
          postDisable = null;
        }) (lib.filterAttrs (_: a: (a.requires or null) != null) den.aspects)
      )
      // r.aspects;

  registryJson =
    host:
    lib.mapAttrs (name: e: {
      inherit (e) desc postEnable postDisable;
      requires = den.lib.requiresOf { inherit host; } den.aspects.${name};
    }) (registryOf host.name);

  extensionFor = hostName: enabled: { den.runtime.${hostName}.enabled = enabled; };

  deltaFor =
    {
      pkgs,
      name,
      base,
      variant,
      baseHomes,
      variantHomes,
    }:
    let
      users = lib.filter (u: baseHomes ? ${u}) (lib.attrNames variantHomes);
      sys = scope: sub: {
        inherit scope;
        base = "${base}/${sub}";
        variant = "${variant}/${sub}";
      };
      home = u: sub: {
        scope = "${sub}:${u}";
        base = "${baseHomes.${u}}/${sub}";
        variant = "${variantHomes.${u}}/${sub}";
      };
      spec = {
        inherit name;
        ignore = [ "^etc:den-runtime/" ];
        scopes = [
          (sys "etc" "etc")
          (sys "daemons" "Library/LaunchDaemons")
          (sys "agents" "Library/LaunchAgents")
          (sys "sw" "sw")
        ]
        ++ lib.concatMap (u: [
          (home u "home-files")
          (home u "home-path")
        ]) users;
        activate = [
          {
            scope = "system";
            base = "${base}/activate";
            variant = "${variant}/activate";
          }
        ]
        ++ map (u: {
          scope = "home:${u}";
          base = "${baseHomes.${u}}/activate";
          variant = "${variantHomes.${u}}/activate";
        }) users;
      };
    in
    pkgs.runCommand "den-runtime-${name}" {
      nativeBuildInputs = [ pkgs.python3 ];
      spec = builtins.toJSON spec;
      passAsFile = [ "spec" ];
    } ''python3 -I ${./runtime.py} delta "$specPath" --out "$out"'';

  cliFor =
    pkgs: host:
    pkgs.writeShellApplication {
      name = "aspect";
      runtimeInputs = [
        pkgs.jq
        pkgs.coreutils
        pkgs.flock
      ];
      text = ''
        host=${lib.escapeShellArg host.name}
        class=${lib.escapeShellArg host.class}
        py=(${pkgs.python3}/bin/python3 -I ${./runtime.py})
        etc=/etc/den-runtime
        registry=$etc/registry.json
        run=/run/den-runtime
        active=$run/active.json
        base=$(readlink -f /run/current-system)
        bundles="''${XDG_STATE_HOME:-$HOME/.local/state}/den-runtime/bundles/$(basename "$base")"
        sudo=sudo
        [ "$(id -u)" = 0 ] && sudo=

        die() { echo "aspect: $*" >&2; exit 1; }
        activeList() { if [ -r "$active" ]; then jq -c 'sort' "$active"; else echo '[]'; fi; }
        known() {
          for n in "$@"; do
            jq -e --arg n "$n" 'has($n)' "$registry" >/dev/null || die "unknown aspect '$n' (see: aspect list)"
          done
        }
        manifest() { echo "$bundles/$1/manifest.json"; }

        probe() {
          local n=$1 failed=0 check script
          while IFS=$'\t' read -r check script; do
            [ -n "$check" ] || continue
            if bash -c "$script" >/dev/null 2>&1; then
              printf 'ok    %s %s\n' "$n" "$check" >&2
            else
              printf 'FAIL  %s %s -- %s\n' "$n" "$check" \
                "$(jq -r --arg n "$n" --arg c "$check" '.[$n].requires[$c].description' "$registry")" >&2
              failed=1
            fi
          done < <(jq -r --arg n "$n" '.[$n].requires | to_entries[] | "\(.key)\t\(.value.script)"' "$registry")
          return "$failed"
        }

        hook() {
          local cmd
          cmd=$(jq -r --arg n "$2" --arg h "$1" '.[$n][$h] // empty' "$registry")
          [ -n "$cmd" ] && nohup bash -c "$cmd" >/dev/null 2>&1 &
          return 0
        }

        # The only command that evaluates: the base plus one aspect, from the
        # exact source the running system was built from.
        build() {
          local n=$1
          mkdir -p "$bundles"
          echo "aspect: building the $n delta against $(basename "$base")" >&2
          # shellcheck disable=SC2016 # the `''${...}` below are Nix, not shell
          flock /tmp/dendritic-eval.lock nix build --impure --out-link "$bundles/$n" \
            --argstr src "path:$(cat "$etc/flake")" --argstr host "$host" --argstr class "$class" \
            --argstr name "$n" --argstr base "$base" --argstr baseHomes "$(cat "$etc/homes.json")" \
            --expr '{ src, host, class, name, base, baseHomes }:
              let
                f = builtins.getFlake src;
                rt = f.debug.config.den.lib.runtime;
                e = f.debug.extendModules { modules = [ (rt.extensionFor host [ name ]) ]; };
                sys = e.config.flake."''${class}Configurations".''${host};
              in
              rt.deltaFor {
                inherit name;
                pkgs = sys.pkgs;
                base = builtins.storePath base;
                variant = sys.config.system.build.toplevel;
                baseHomes = builtins.mapAttrs (_: builtins.storePath) (builtins.fromJSON baseHomes);
                variantHomes = builtins.mapAttrs (_: u: u.home.activationPackage) (sys.config.home-manager.users or { });
              }' || die "$n: delta has conflicts or failed to build (see above)"
        }

        apply() {
          local want=$1 out manifests=()
          for n in $(jq -r '.[]' <<<"$want"); do
            [ -e "$(manifest "$n")" ] || build "$n"
            manifests+=("$(manifest "$n")")
          done
          $sudo mkdir -p "$run"
          out=$($sudo "''${py[@]}" reconcile "''${manifests[@]}") || exit $?
          echo "$out"
          if grep -q '/etc/nix/nix.conf$' <<<"$out"; then
            $sudo launchctl kickstart -k system/org.nixos.nix-daemon || true
          fi
          jq -cn --argjson w "$want" '$w' | $sudo tee "$active.tmp" >/dev/null
          $sudo mv "$active.tmp" "$active"
        }

        notes() {
          jq -r '.activation[] | "note: \(.scope) activation also differs (not applied; use postEnable):", (.diff[:12][] | "  " + rtrimstr("\n"))' \
            "$(manifest "$1")" >&2
        }

        exec 9>/tmp/den-runtime.lock
        flock 9

        force=0 args=()
        cmd=''${1:-list}
        shift || true
        mode=
        for a in "$@"; do
          case "$a" in
            -f | --force) force=1 ;;
            --activation) mode=activation ;;
            *) args+=("$a") ;;
          esac
        done

        case "$cmd" in
          list | status)
            [ "''${#args[@]}" -gt 0 ] || mapfile -t args < <(jq -r 'keys[]' "$registry")
            known "''${args[@]}"
            rc=0
            for n in "''${args[@]}"; do
              if jq -e --arg n "$n" 'index($n) != null' <<<"$(activeList)" >/dev/null; then m='[✓]'; else m='[ ]'; rc=1; fi
              if [ -e "$(manifest "$n")" ]; then
                b=$(jq -r '"+\(.add|length) ~\(.shared|length) !\(.conflicts|length)"' "$(manifest "$n")")
              else
                b="not built"
              fi
              printf '%s %-20s %-16s %s\n' "$m" "$n" "$b" "$(jq -r --arg n "$n" '.[$n].desc' "$registry")"
            done
            [ "$cmd" = status ] && exit "$rc"
            exit 0
            ;;
          check)
            for n in "''${args[@]}"; do
              known "$n"
              [ -e "$(manifest "$n")" ] || build "$n"
              jq -r '"\(.name): adds", (.add[] | "  + \(.scope):\(.rel)"), "extends", (.shared[] | "  ~ \(.scope):\(.rel)"), "conflicts", (.conflicts[] | "  ! \(.scope):\(.rel) \(.reason)")' "$(manifest "$n")"
              notes "$n"
            done
            ;;
          probe)
            [ "''${#args[@]}" -gt 0 ] || mapfile -t args < <(jq -r 'keys[]' "$registry")
            known "''${args[@]}"
            rc=0
            for n in "''${args[@]}"; do probe "$n" || rc=1; done
            exit "$rc"
            ;;
          build)
            [ "''${#args[@]}" -gt 0 ] || mapfile -t args < <(jq -r 'keys[]' "$registry")
            known "''${args[@]}"
            for n in "''${args[@]}"; do build "$n"; done
            ;;
          up)
            [ "''${#args[@]}" -gt 0 ] || die "usage: aspect up <aspects...> [--force]"
            known "''${args[@]}"
            if [ "$force" != 1 ]; then
              for n in "''${args[@]}"; do probe "$n" || die "'$n' fails its requires; --force to load anyway"; done
            fi
            apply "$(activeList | jq -c --args '. + $ARGS.positional | unique' "''${args[@]}")"
            for n in "''${args[@]}"; do notes "$n"; hook postEnable "$n"; done
            ;;
          down)
            [ "''${#args[@]}" -gt 0 ] || die "usage: aspect down <aspects...>"
            known "''${args[@]}"
            apply "$(activeList | jq -c --args '. - $ARGS.positional | sort' "''${args[@]}")"
            for n in "''${args[@]}"; do hook postDisable "$n"; done
            ;;
          reset)
            if [ "$mode" = activation ]; then
              mkdir -p "$run"
              "''${py[@]}" reconcile
              rm -f "$active"
            else
              apply '[]'
            fi
            ;;
          *) die "commands: list, status, check, probe, build, up, down, reset" ;;
        esac
      '';
    };
in
{
  options.den.runtime = mkOption {
    type = types.attrsOf hostType;
    default = { };
    description = "Per host, the aspects `aspect up`/`aspect down` toggle at runtime.";
  };

  config.den.lib.runtime = {
    inherit
      registryOf
      extensionFor
      deltaFor
      cliFor
      ;
  };

  config.flake.runtimeAspects = lib.filterAttrs (_: r: r != { }) (
    lib.mapAttrs (_: registryJson) hostsOf
  );

  config.den.aspects = lib.mkMerge [
    (lib.mapAttrs (hostName: r: {
      excludes = map (n: den.aspects.${n}) (
        lib.filter (n: !lib.elem n r.enabled) (lib.attrNames (registryOf hostName))
      );
    }) config.den.runtime)
    {
      den-runtime.os =
        {
          pkgs,
          config,
          host ? { },
          ...
        }:
        let
          registry = lib.optionalAttrs (host ? name) (registryJson host);
          cli = cliFor pkgs host;
        in
        lib.mkIf (registry != { }) (
          lib.mkMerge [
            {
              environment.systemPackages = [ cli ];
              environment.etc."den-runtime/flake".text = self.outPath;
              environment.etc."den-runtime/registry.json".text = builtins.toJSON registry;
              environment.etc."den-runtime/homes.json".text = builtins.toJSON (
                lib.mapAttrs (_: u: u.home.activationPackage) (config.home-manager.users or { })
              );
            }
            (lib.optionalAttrs ((host.class or "") == "darwin") {
              environment.systemPath = [
                "/run/den-runtime/sw/bin"
                "$HOME/.local/state/den-runtime/home-path/bin"
              ];
              system.activationScripts.preActivation.text = "${cli}/bin/aspect reset --activation || true";
            })
          ]
        );
    }
  ];

  config.den.schema.host.includes = [ den.aspects.den-runtime ];

  config.perSystem =
    { pkgs, ... }:
    {
      checks.runtime-aspects =
        pkgs.runCommand "runtime-aspects-tests" { nativeBuildInputs = [ pkgs.python3 ]; }
          ''
            cp ${./runtime.py} runtime.py
            cp ${./test_runtime.py} test_runtime.py
            python3 -I test_runtime.py
            touch $out
          '';
    };
}
