# The iPhone: a real den host on a system nix has never heard of.
#
# It resolves an aspect and has a name and a system like any host, but lands in
# `mobileConfigurations` instead of a system closure -- see
# ../framework/mobile.nix for how the `mobile` class does that, and why
# ../tooling/ci-matrix.nix therefore never tries to build a phone.
#
# `mobile` is the general class; `aarch64-ios` is one label under it, and this
# phone is its only instance so far.
#
#   nix run .#iphone-deploy                      mint a tailnet key, then pair t3code
#   nix eval --raw .#mobileConfigurations.iphone.bundle
{
  lib,
  config,
  ...
}:
let
  registry = import ./_registry.nix;
  name = "iphone";
  # perSystem rebinds `config` to its own scope; the device's merged manifest
  # lives on the flake-level one.
  flakeConfig = config;
in
assert lib.assertMsg (registry ? ${name} && registry.${name} ? device)
  "aspects/hosts/iphone.nix: '${name}' must be a `device` entry in ./_registry.nix (../network/tailscale.nix builds its client list from those)";
{
  # FLAT form on purpose. den only treats a key as a system group when it is in
  # `lib.systems.flakeExposed`, so `den.hosts.aarch64-ios.iphone` is read as a
  # host *named* aarch64-ios and throws. Declared flat, den groups it itself and
  # never asks whether the system is real.
  den.hosts.${name} = {
    system = "aarch64-ios";

    # `ios` is the concrete class; aspects write `mobile` and it is forwarded
    # here, the way `os` forwards into `nixos`/`darwin`. See ../framework/mobile.nix.
    #
    # Both overrides below are required, not stylistic: they stop den's
    # class-indexed defaults (which throw on an unknown class) being forced.
    class = "ios";
    instantiate = config.flake.lib.mobileSystem {
      inherit name;
      system = "aarch64-ios";
    };
    intoAttr = [
      "mobileConfigurations"
      name
    ];
  };

  # Ordinary aspect content in the `mobile` class -- the point of doing this as
  # a den host: anything in the tree can contribute to the phone normally.
  den.aspects.${name}.mobile = {
    device.description = registry.${name}.device.description;
    device.deviceName = "Ivy’s iPhone";
    device.tailscale.enable = true;

    # No t3code web clip: there is a real t3code iOS app. It comes from the App
    # Store, so nothing here installs it -- `.#t3code-pair` points it at this
    # machine once it is on the tailnet. `device.webClips` stays available for
    # tailnet services that have no app of their own.

    device.notes = [
      "nix run .#iphone-deploy          -- mint a tailnet key, then pair t3code"
      "nix run .#iphone-apps            -- install a signed .app (or .ipa) with devicectl"
      "nix run .#iphone-webclips        -- push home-screen web clips over Taildrop"
      "nix run .#tailscale-client-key -- ${name}   -- just the key"
      "nix run .#t3code-pair            -- just the t3code pairing QR"
      "The key is single-use and preauthorized: it is spent once the phone enrols."
      "t3code itself is the App Store app, not a web clip and not deployed here."
    ];
  };

  perSystem =
    { pkgs, config, ... }:
    {
      packages.iphone-deploy = pkgs.writeShellApplication {
        name = "iphone-deploy";
        runtimeInputs = [
          config.packages.tailscale-client-key
          config.packages.t3code-pair
          pkgs.coreutils
        ];
        text = ''
          echo "==> minting a tailnet key for ${name}" >&2

          # Clipboard, not scrollback: the key is live until the phone spends
          # it, and the pasteboard is where it has to end up anyway.
          key="$(tailscale-client-key ${name})"

          if command -v pbcopy >/dev/null 2>&1; then
            printf '%s' "$key" | pbcopy
            echo "    key copied to the clipboard (not printed)" >&2
          else
            echo "    no pbcopy; the key is on stdout below" >&2
            printf '%s\n' "$key"
          fi
          unset key

          echo "" >&2
          echo "==> Tailscale app -> Custom auth key -> paste, then come back" >&2
          echo "" >&2
          echo "==> t3code pairing" >&2

          # Not fatal: failing here would throw away the key just minted.
          if ! t3code-pair; then
            echo "" >&2
            echo "    t3code is not pairable yet -- the key above is still good." >&2
            echo "    Re-run 'nix run .#t3code-pair' once 't3 serve' is running." >&2
          fi
        '';
      };

      apps.iphone-deploy = {
        type = "app";
        program = lib.getExe config.packages.iphone-deploy;
      };

      # Native apps. `devicectl` ships with Xcode 15+, not the command line
      # tools, and it only installs signed payloads -- so this deploys what it
      # is given and does not pretend to sign anything.
      packages.iphone-apps = pkgs.writeShellApplication {
        name = "iphone-apps";
        runtimeInputs = [
          pkgs.jq
          pkgs.unzip
          pkgs.findutils
          pkgs.coreutils
        ];
        text = ''
          manifest=${pkgs.writeText "${name}-manifest.json" flakeConfig.flake.mobileConfigurations.${name}.manifest}

          if ! xcrun --find devicectl >/dev/null 2>&1; then
            echo "iphone-apps: devicectl not found -- it ships with Xcode 15+, not the CLT." >&2
            exit 1
          fi

          count="$(jq -r '.apps | length' "$manifest")"
          if [ "$count" -eq 0 ]; then
            echo "iphone-apps: no apps declared in den.aspects.${name}.mobile.device.apps" >&2
            exit 0
          fi

          devices="$(mktemp)"
          trap 'rm -f "$devices"' EXIT
          xcrun devicectl list devices --json-output "$devices" >/dev/null

          want="''${1:-$(jq -r '.deviceName' "$manifest")}"

          # Apple writes a curly apostrophe in the device name; anyone typing it
          # writes a straight one. Strip everything that is not alphanumeric
          # rather than enumerate quote characters -- a literal curly quote in
          # this script trips shellcheck SC1112.
          udid="$(jq -r --arg n "$want" '
            def norm: ascii_downcase | gsub("[^a-z0-9]"; "");
            .result.devices[]
            | select((.deviceProperties.name // "" | norm) == ($n | norm))
            | .identifier' "$devices" | head -n1)"

          if [ -z "$udid" ] || [ "$udid" = "null" ]; then
            echo "iphone-apps: no paired device named '$want'." >&2
            echo "  paired devices:" >&2
            jq -r '.result.devices[] | "    " + (.deviceProperties.name // "?") ' "$devices" >&2
            echo "  Pair it in Xcode, and enable Developer Mode on the device." >&2
            echo "  Pass a name as the first argument to target a different one." >&2
            exit 1
          fi

          echo "iphone-apps: target $want ($udid)" >&2

          jq -r '.apps | to_entries[] | [.key, .value.source, (.value.bundleId // ""), (.value.launch|tostring)] | @tsv' "$manifest" \
          | while IFS=$'\t' read -r key source bundle launch; do
              # devicectl takes a .app bundle only. An .ipa is a zip with the
              # bundle at Payload/<Name>.app, so unwrap it rather than make
              # every caller do so.
              case "$source" in
                *.ipa)
                  stage="$(mktemp -d)"
                  unzip -q "$source" -d "$stage"
                  source="$(find "$stage/Payload" -maxdepth 1 -name '*.app' | head -n1)"
                  if [ -z "$source" ]; then
                    echo "iphone-apps: $key: no Payload/*.app inside the .ipa" >&2
                    exit 1
                  fi
                  ;;
              esac

              echo "==> installing $key" >&2
              xcrun devicectl device install app --device "$udid" "$source"
              if [ "$launch" = "true" ] && [ -n "$bundle" ]; then
                echo "==> launching $bundle" >&2
                xcrun devicectl device process launch --device "$udid" "$bundle"
              fi
            done
        '';
      };

      apps.iphone-apps = {
        type = "app";
        program = lib.getExe config.packages.iphone-apps;
      };

      # Home-screen web clips, as an unsigned configuration profile pushed over
      # Taildrop. Unsigned installs fine; iOS shows it as unverified.
      packages.iphone-webclips = pkgs.writeShellApplication {
        name = "iphone-webclips";
        runtimeInputs = [
          pkgs.jq
          pkgs.python3
          pkgs.tailscale
          pkgs.coreutils
        ];
        text = ''
          manifest=${pkgs.writeText "${name}-manifest.json" flakeConfig.flake.mobileConfigurations.${name}.manifest}

          if [ "$(jq -r '.webClips | length' "$manifest")" -eq 0 ]; then
            echo "iphone-webclips: none declared in device.webClips" >&2
            exit 0
          fi

          if ! tailscale status >/dev/null 2>&1; then
            echo "iphone-webclips: tailscale is not up; cannot resolve URLs or send." >&2
            exit 1
          fi

          suffix="$(tailscale status --json | jq -r '.MagicDNSSuffix')"
          selfdns="$(tailscale status --json | jq -r '.Self.DNSName' | sed 's/\.$//')"

          out="$(mktemp -d)/webclips.mobileconfig"
          python3 ${./_webclips.py} "$manifest" "$suffix" "$selfdns" "$out"

          # Taildrop targets the tailnet name, which is neither the den host
          # name nor the device's own name.
          target="''${1:-$(jq -r '.hostname' "$manifest")}"

          echo "iphone-webclips: sending $out to $target over Taildrop" >&2
          if tailscale file cp "$out" "$target:"; then
            echo "    accept it on the phone, then Settings -> Profile Downloaded -> Install" >&2
          else
            echo "    Taildrop failed; the profile is at: $out" >&2
            echo "    AirDrop or email it to the phone instead." >&2
            exit 1
          fi
        '';
      };

      apps.iphone-webclips = {
        type = "app";
        program = lib.getExe config.packages.iphone-webclips;
      };
    };
}
