# The iPhone: a real den host on a system nix has never heard of.
#
# It resolves an aspect and has a name and a system like any host, but lands in
# `mobileConfigurations` instead of a system closure -- see
# ../framework/mobile/ for how the `mobile` class does that, and why
# ../tooling/ci-matrix.nix therefore never tries to build a phone.
#
# `mobile` is the general class; `aarch64-ios` is one label under it, and this
# phone is its only instance so far.
#
#   nix run .#iphone-deploy                      mint a tailnet key, then pair t3code
#   nix run .#mobileConfigurations.iphone.toplevel   activate the current generation
#   nix eval --raw .#mobileConfigurations.iphone.bundle
{
  lib,
  den,
  config,
  ...
}:
let
  name = "iphone";
  description = "iPhone (Tailscale app)";
in
{
  # FLAT form on purpose. den only treats a key as a system group when it is in
  # `lib.systems.flakeExposed`, so `den.hosts.aarch64-ios.iphone` is read as a
  # host *named* aarch64-ios and throws. Declared flat, den groups it itself and
  # never asks whether the system is real.
  den.hosts.${name} = {
    system = "aarch64-ios";

    # A tailscale client with nothing built for it: ../network/tailscale.nix
    # offers it to `.#tailscale-client-key`.
    device = { inherit description; };

    # Puts it in her household, which is what the capability resolver reaches across.
    owner = den.persons.ivy;

    # `ios` is the concrete class; aspects write `mobile` and it is forwarded
    # here, the way `os` forwards into `nixos`/`darwin`. See ../framework/mobile/.
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
    device.description = description;
    device.deviceName = "Ivy’s iPhone";
    device.tailscale.enable = true;

    # No t3code web clip: there is a real t3code iOS app. It comes from the App
    # Store, so nothing here installs it -- `.#t3code-pair` points it at this
    # machine once it is on the tailnet. `profile.payloads.<n>.webClip` is for
    # tailnet services that have no app of their own.

    device.notes = [
      "nix run .#iphone-deploy          -- mint a tailnet key, then pair t3code"
      "nix run .#mobileConfigurations.${name}.toplevel -- serve this generation; run 'Activate ${name}' on the phone"
      "nix run .#tailscale-client-key -- ${name}   -- just the key"
      "nix run .#t3code-pair            -- just the t3code pairing QR"
      "The key is single-use and preauthorized: it is spent once the phone enrols."
      "t3code itself is the App Store app, not a web clip and not deployed here."
    ];
  };

  perSystem =
    {
      pkgs,
      config,
      ...
    }:
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
    };
}
