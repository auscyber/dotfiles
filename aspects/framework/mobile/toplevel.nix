{ lib, ... }:
let
  inherit (lib) types mkOption;
in
{
  config.mobile.modules.toplevel =
    {
      config,
      name,
      pkgs,
      ...
    }:
    let
      b = config.build;
      write = pkgs.writeText;

      wallpaperMime =
        p:
        {
          png = "image/png";
          jpg = "image/jpeg";
          jpeg = "image/jpeg";
          heic = "image/heic";
        }
        .${lib.toLower (lib.last (lib.splitString "." (toString p)))};

      signedApps = lib.mapAttrs (_: a: {
        inherit (a) bundleId;
        source = toString a.from.signed;
      }) (lib.filterAttrs (_: a: a.from ? signed) config.apps);

      render = pkgs.writers.writePython3Bin "mobile-render" { flakeIgnore = [ "E501" ]; } ''
        import json
        import os
        import re
        import subprocess
        import sys
        from xml.sax.saxutils import escape

        src, dst, mode = sys.argv[1:4]
        with open(os.environ["MOBILE_SECRETS"]) as f:
            commands = json.load(f)
        fixed = {k: os.environ["MOBILE_" + k] for k in ("BASE", "TAILNET", "SELF")}
        cache = {}


        def value(m):
            key = m.group(1)
            if key.startswith("secret:"):
                n = key[len("secret:"):]
                if n not in cache:
                    out = subprocess.run(commands[n], check=True, stdout=subprocess.PIPE, text=True)
                    cache[n] = out.stdout.rstrip("\n")
                    if not cache[n]:
                        sys.exit(f"mobile-render: secret {n} printed nothing")
                v = cache[n]
            else:
                v = fixed[key]
            if mode == "xml":
                return escape(v)
            if mode == "json":
                return json.dumps(v)[1:-1]
            return v


        with open(src) as f:
            text = f.read()
        text = re.sub(r"@(BASE|TAILNET|SELF|secret:[^@]+)@", value, text)
        fd = os.open(dst, os.O_WRONLY | os.O_CREAT | os.O_TRUNC, 0o600)
        with os.fdopen(fd, "w") as f:
            f.write(text)
      '';

      serve = pkgs.writers.writePython3Bin "mobile-serve" { flakeIgnore = [ "E501" ]; } ''
        import functools
        import http.server
        import os
        import sys

        root, portfile = sys.argv[1:3]


        class Handler(http.server.SimpleHTTPRequestHandler):
            extensions_map = {
                ".mobileconfig": "application/x-apple-aspen-config",
                ".json": "application/json",
                "": os.environ.get("MOBILE_WALLPAPER_TYPE", "application/octet-stream"),
            }


        server = http.server.ThreadingHTTPServer(("127.0.0.1", 0), functools.partial(Handler, directory=root))
        with open(portfile, "w") as f:
            f.write(str(server.server_address[1]))
        server.serve_forever()
      '';

      activate = pkgs.writeShellApplication {
        name = "activate";
        # The device name carries Apple's curly apostrophe, inside single quotes.
        excludeShellChecks = [ "SC1112" ];
        runtimeInputs = [
          render
          serve
          pkgs.jq
          pkgs.tailscale
          pkgs.coreutils
          pkgs.unzip
          pkgs.findutils
          pkgs.gnused
        ];
        text = ''
          self="$(cd "$(dirname "$0")/.." && pwd)"
          device=${lib.escapeShellArg config.device.deviceName}
          path=${lib.escapeShellArg config.activation.path}
          state="''${XDG_STATE_HOME:-$HOME/.local/state}/dendritic-mobile/${name}"
          reimport=0
          [ "''${1:-}" = "--reimport" ] && reimport=1

          work="$(mktemp -d)"
          chmod 700 "$work"
          mkdir -p "$work/www" "$work/unsigned" "$work/signed" "$state"
          server=""
          served=0
          cleanup() {
            [ "$served" = 1 ] && tailscale serve --https=443 --set-path="$path" off >/dev/null 2>&1
            [ -n "$server" ] && kill "$server" 2>/dev/null
            rm -rf "$work"
          }
          trap cleanup EXIT

          if ! tailscale status >/dev/null 2>&1; then
            echo "activate: tailscale is not up" >&2
            exit 1
          fi
          ts="$(tailscale status --json)"
          MOBILE_TAILNET="$(jq -r .MagicDNSSuffix <<<"$ts")"
          MOBILE_SELF="$(jq -r .Self.DNSName <<<"$ts" | sed 's/\.$//')"
          MOBILE_BASE="https://$MOBILE_SELF$path"
          MOBILE_SECRETS=${write "secrets.json" (builtins.toJSON b.secretCommands)}
          export MOBILE_TAILNET MOBILE_SELF MOBILE_BASE MOBILE_SECRETS
          ${lib.optionalString (config.wallpaper != null) ''
            export MOBILE_WALLPAPER_TYPE=${wallpaperMime config.wallpaper.image}
            cp "$self/wallpaper" "$work/www/wallpaper"
          ''}

          [ -f "$self/profile.mobileconfig" ] && mobile-render "$self/profile.mobileconfig" "$work/www/profile.mobileconfig" xml
          mobile-render "$self/manifest.json" "$work/manifest.json" json

          installed="[]"
          if xcrun devicectl device info apps --device "$device" --timeout 15 -q -j "$work/apps.json" >/dev/null 2>&1; then
            installed="$(jq '[.result.apps[]?.bundleIdentifier]' "$work/apps.json")"
            jq -r 'to_entries[] | [.key, .value.bundleId, .value.source] | @tsv' "$self/signed-apps.json" \
            | while IFS=$'\t' read -r key bundle source; do
                case "$source" in
                  *.ipa)
                    stage="$(mktemp -d -p "$work")"
                    unzip -q "$source" -d "$stage"
                    source="$(find "$stage/Payload" -maxdepth 1 -name '*.app' | head -n1)"
                    ;;
                esac
                echo "==> devicectl: installing $key ($bundle)" >&2
                xcrun devicectl device install app --device "$device" "$source"
              done
          else
            echo "activate: $device is not reachable over devicectl; App Store links and signed apps skipped" >&2
          fi
          jq --argjson have "$installed" \
            '.open += [.apps[] | select(.bundleId as $b | $have | index($b) | not) | .storeUrl] | del(.apps)' \
            "$work/manifest.json" > "$work/www/manifest.json"

          for f in "$self"/shortcuts/*.plist; do
            [ -e "$f" ] || continue
            n="$(basename "$f" .plist)"
            mobile-render "$f" "$work/unsigned/$n.shortcut" xml
            sum="$(sha256sum "$work/unsigned/$n.shortcut" | cut -d' ' -f1)"
            if [ "$reimport" = 1 ] || [ "$(cat "$state/$n.sha256" 2>/dev/null)" != "$sum" ]; then
              shortcuts sign -m people-who-know-me -i "$work/unsigned/$n.shortcut" -o "$work/signed/$n.shortcut"
              open "$work/signed/$n.shortcut"
              printf '%s\n' "$sum" > "$state/$n.sha256"
              echo "==> shortcut '$n' changed: Add it on this Mac; iCloud syncs it to the device" >&2
            fi
          done

          mobile-serve "$work/www" "$work/port" &
          server=$!
          until [ -s "$work/port" ]; do sleep 0.1; done
          tailscale serve --bg --https=443 --set-path="$path" "http://127.0.0.1:$(cat "$work/port")" >/dev/null
          served=1

          echo "==> generation $(jq -r .generation "$work/www/manifest.json") served at $MOBILE_BASE" >&2
          echo "    run '${config.activation.shortcut}' on the device; Ctrl-C here when it is done" >&2
          wait "$server"
        '';
      };
    in
    {
      options.build.system = mkOption {
        type = types.enum [
          "aarch64-darwin"
          "x86_64-darwin"
        ];
        default = "aarch64-darwin";
        description = "The Mac that builds and activates generations; signing needs its iCloud identity.";
      };

      options.build.toplevel = mkOption {
        type = types.package;
        readOnly = true;
      };

      config.assertions =
        lib.mapAttrsToList (n: _: {
          assertion = !lib.hasInfix "/" n;
          message = "mobile ${name}: shortcut name '${n}' becomes a file name and may not contain '/'.";
        }) config.shortcuts
        ++ lib.optional (config.wallpaper != null) {
          assertion =
            builtins.match ".*\\.(png|jpe?g|heic)" (lib.toLower (toString config.wallpaper.image)) != null;
          message = "mobile ${name}: wallpaper must be png, jpeg or heic.";
        };

      config.build.toplevel =
        let
          failed = map (a: a.message) (lib.filter (a: !a.assertion) config.assertions);
        in
        if failed != [ ] then
          throw "\nFailed assertions:\n${lib.concatMapStringsSep "\n" (m: "- ${m}") failed}"
        else
          pkgs.runCommand "mobile-${name}"
            {
              nativeBuildInputs = [ pkgs.jq ];
              meta.mainProgram = "activate";
            }
            ''
              mkdir -p "$out/shortcuts" "$out/bin"
              ${lib.optionalString (b.profile != null) ''
                cp ${write "profile.mobileconfig" b.profile} "$out/profile.mobileconfig"
              ''}
              ${lib.concatStrings (
                lib.mapAttrsToList (n: xml: ''
                  cp ${write "shortcut.plist" xml} "$out/shortcuts/"${lib.escapeShellArg "${n}.plist"}
                '') b.shortcuts
              )}
              ${lib.optionalString (config.wallpaper != null) ''
                cp ${config.wallpaper.image} "$out/wallpaper"
              ''}
              cp ${write "signed-apps.json" (builtins.toJSON signedApps)} "$out/signed-apps.json"
              manifest=${write "manifest.json" (builtins.toJSON b.manifest)}
              gen="$( (cd "$out" && find . -type f -print0 | sort -z | xargs -0 sha256sum; cat "$manifest") | sha256sum | cut -c1-12)"
              jq --arg g "$gen" '.generation = $g' "$manifest" > "$out/manifest.json"
              ln -s ${lib.getExe activate} "$out/bin/activate"
            '';
    };
}
