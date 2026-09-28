{
  den,
  lib,
  ...
}:
let
  # Both HM modules declare `package` as nullable. A null means "the module
  # installs nothing, the binary comes from elsewhere", so fall back to the bare
  # name -- which resolves, because the matching `enable<X>` override below
  # prefixes that package onto t3code's own PATH.
  binOf = pkg: fallback: if pkg == null then fallback else lib.getExe pkg;
in
{
  den.aspects.t3code = {
    homeManager =
      {
        config,
        pkgs,
        host,
        ...
      }:
      {
        options.t3code.runtimeTools = lib.mkOption {
          type = lib.types.attrsOf lib.types.bool;
          default = { };
          description = ''
            `pkgs.t3code.override` flags, accumulated by the per-agent blocks
            below. An attrset rather than a list so two blocks asking for the
            same tool merge instead of colliding, and so the value can be handed
            to `override` unchanged.
          '';
        };

        # Each agent block applies only if that aspect is on the host, and
        # contributes two things: the `pkgs.t3code.override` flag that puts the
        # agent's binary on t3code's PATH, and the matching `providerInstances`
        # entry pointing at it. Without the override the provider is configured
        # but unlaunchable; without the instance the binary is present but
        # unused.
        config = lib.mkMerge [
          {
            programs.t3code = {
              enable = true;
              package = pkgs.t3code.override config.t3code.runtimeTools;

              # The activation script jq-merges these over whatever t3code wrote
              # itself, so settings changed in the GUI survive a switch and only
              # the keys named here are forced back.
              mutableUserSettings = true;
              mutableClientSettings = true;

              userSettings = {
                enableAssistantStreaming = true;
              };

              clientSettings.settings = {
                sidebarProjectGroupingMode = "repository";
                timestampFormat = "locale";
              };
            };
          }

          (lib.mkIf (host.hasAspect den.aspects.claude) {
            t3code.runtimeTools.enableClaude = true;
            programs.t3code.userSettings.providerInstances.claude = {
              driver = "claudeAgent";
              displayName = "Claude Code";
              enabled = true;
              config = {
                # `enabled` twice is not a mistake, and it is what the module's
                # own `userSettings` example does. The outer one gates the
                # *instance*; this one is the claudeAgent driver's own setting,
                # carried over from the legacy `providers.<kind>` block that
                # t3code still migrates from. Setting only the outer one leaves
                # the driver reading a default of `false` on that path.
                enabled = true;
                binaryPath = binOf config.programs.claude-code.package "claude";
                # Empty, not unset: the driver treats "" as "use the default"
                # and a missing key as a parse failure on some versions.
                homePath = "";
                launchArgs = "";
                autoCompactWindow = "";
                customModels = [ ];
              };
            };
          })

          # `serverUrl = ""` is what makes t3code spawn its own `opencode serve`
          # on demand; setting it would point the driver at an externally
          # managed server instead, which nothing here runs.
          (lib.mkIf (host.hasAspect den.aspects.opencode) {
            t3code.runtimeTools.enableOpencode = true;
            programs.t3code.userSettings.providerInstances.opencode = {
              driver = "opencode";
              displayName = "opencode";
              enabled = true;
              config = {
                # Same doubling as the claude instance above -- see the note there.
                enabled = true;
                binaryPath = binOf config.programs.opencode.package "opencode";
                serverUrl = "";
                serverPassword = "";
                customModels = [ ];
              };
            };
          })

          (lib.mkIf (host.hasAspect den.aspects.jujutsu) {
            t3code.runtimeTools.enableJujutsu = true;
          })

          # Remote access. `t3 serve --tailscale-serve` puts the UI behind the
          # tailnet's own HTTPS at https://<machine>.<tailnet>.ts.net/ -- which
          # is what mobile needs, since app.t3.codes refuses a plain-HTTP origin.
          #
          # Deliberately NOT gated on den.aspects.vpn: the wireguard tunnel only
          # works while the server at `vpn.endpoint` is reachable, and the whole
          # point of reaching this from a phone is that it works when it is not.
          # Tailscale falls back to a DERP relay, so the laptop stays reachable
          # from anywhere without depending on that box being up.
          (lib.mkIf (host.hasAspect den.aspects.tailscale) {
            launchd.agents.t3code-serve = {
              enable = true;
              config = {
                ProgramArguments = [
                  "${config.programs.t3code.package}/bin/t3"
                  "serve"
                  "--tailscale-serve"
                ];
                RunAtLoad = true;
                KeepAlive = true;
                # `t3 serve --tailscale-serve` shells out to the `tailscale`
                # CLI to publish the mapping, and a launchd agent inherits
                # almost nothing, so the CLI has to be named on PATH here.
                EnvironmentVariables.PATH = lib.makeBinPath [
                  pkgs.tailscale
                  pkgs.coreutils
                ];
                StandardOutPath = "${config.home.homeDirectory}/Library/Logs/t3code-serve.log";
                StandardErrorPath = "${config.home.homeDirectory}/Library/Logs/t3code-serve.log";
              };
            };
          })
        ];
      };
  };

  # `nix run .#t3code-pair` -- get the phone onto this machine's t3code.
  #
  # Two separate things have to be true before a phone can reach t3code, and
  # this app is the second of them:
  #
  #   1. the phone is on the tailnet  -> nix run .#tailscale-client-key -- mobile
  #   2. the phone is paired to t3code -> this
  #
  # `t3 pair --tailscale` makes the mapping persistent, so it survives a
  # restart of the server rather than handing out a link that dies with the
  # process. It has to run on the machine t3code is serving from, against the
  # already-running `t3 serve --tailscale-serve` agent -- it does not start one.
  #
  # The URL is printed as a QR as well as text, because the whole point is to
  # get it into a phone, and https://<machine>.<tailnet>.ts.net/... is not
  # something anyone wants to retype. `-t ANSIUTF8` renders with half-block
  # characters so it stays scannable in a normal-sized terminal.
  perSystem =
    {
      pkgs,
      config,
      ...
    }:
    {
      # Package as well as app: writeShellApplication runs shellcheck in its
      # builder, and an `apps.<x>.program` string cannot be built directly, so
      # app-only scripts are never linted. `nix build .#t3code-pair` does.
      packages.t3code-pair = pkgs.writeShellApplication {
        name = "t3code-pair";
        runtimeInputs = [
          pkgs.t3code
          pkgs.tailscale
          pkgs.qrencode
          pkgs.coreutils
          pkgs.gnugrep
        ];
        text = ''
          if ! tailscale status >/dev/null 2>&1; then
            echo "t3code-pair: tailscale is not up -- nothing to pair over." >&2
            echo "  check: sudo launchctl list | grep tailscale" >&2
            echo "  log:   /var/log/tailscale-autoconnect.log" >&2
            exit 1
          fi

          echo "t3code-pair: asking the running t3 server for a tailnet pairing..." >&2
          out="$(t3 pair --tailscale "$@")" || {
            echo "t3code-pair: 't3 pair --tailscale' failed." >&2
            echo "  Is the t3code-serve agent running? It is only present when" >&2
            echo "  den.aspects.tailscale is on this host -- see aspects/dev/llms/t3code.nix." >&2
            exit 1
          }

          printf '%s\n' "$out"

          # Pull the first https URL back out of whatever t3 printed rather
          # than assuming a fixed output shape -- the text around it has
          # changed between t3 versions, the URL has not.
          url="$(printf '%s' "$out" | grep -oE 'https://[^[:space:]]+' | head -n1 || true)"
          if [ -n "$url" ]; then
            echo "" >&2
            qrencode -t ANSIUTF8 "$url"
            echo "Scan with the phone, or open: $url" >&2
          fi
        '';
      };

      apps.t3code-pair = {
        type = "app";
        program = lib.getExe config.packages.t3code-pair;
      };
    };
}
