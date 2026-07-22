{
  den,
  inputs,
  ...
}:
{
  flake-file.inputs.nix-openclaw = {
    url = "github:openclaw/nix-openclaw";
    inputs.nixpkgs.follows = "nixpkgs";
    inputs.home-manager.follows = "home-manager";
  };

  # nix-openclaw pins OpenClaw-2026.7.1.zip's hash from 07-14 05:30; upstream
  # re-signed and re-uploaded the macOS artifacts at 06:49, so the pin no longer
  # matches what GitHub serves and upstream's own flake cannot build openclaw-app.
  # The served zip is notarized Developer ID (OpenClaw Foundation, FWJYW4S8P8),
  # so the bytes are authentic; only the recorded hash is stale. Drop this once
  # upstream re-pins.
  patchedInputs.nix-openclaw = { };

  den.aspects.openclaw = {
    includes = [
      den.aspects.mcp-servers
      den.aspects.agenix-rekey
    ];

    # nix-openclaw's HM module resolves pkgs.openclawPackages from its overlay.
    #
    # That overlay splats its whole package set onto the top level, including a
    # pinned pnpm_11 (11.2.2) that replaces nixpkgs'. 11.2.2 predates pnpm's
    # `trust-lockfile` setting, so it ignores the pnpm_config_trust_lockfile
    # that pnpmConfigHook exports to skip supply-chain verification -- every
    # other pnpm package in the config (sketchybar-app-font) then verifies the
    # lockfile online mid-build and hangs. openclaw's own packages are built
    # from `prev` and take pnpm_11 as an explicit argument, so dropping the
    # top-level attr leaves them untouched; it stays at
    # pkgs.openclawPackages.pnpm_11.
    # Trailing `tr -d '\n'`: `base64` line-wraps at 76 columns, and 32 hex-encoded
    # bytes encode to 88 characters, so without it the token is emitted as two
    # lines. agenix substitutes a dependency's embedded newlines literally (only
    # the *trailing* one is stripped, by `$(...)`), which would split the
    # KEY=VALUE template below across two lines and truncate the token.
    secrets.openclaw-token.generator.script = { pkgs, ... }: ''
      openssl rand -hex 32 | tr -d '\n' | base64 | tr -d '\n'
    '';
    overlays.nix-openclaw =
      final: prev: removeAttrs (inputs.nix-openclaw.overlays.default final prev) [ "pnpm_11" ];

    # openclaw is flagged insecure in nixpkgs.
    darwin = { lib, ... }: {
      nixpkgs.config.allowInsecurePredicate = pkg: lib.hasPrefix "openclaw" (lib.getName pkg);
    };

    homeManager =
      {
        config,
        lib,
        pkgs,
        ...
      }:
      {
        imports = [ inputs.nix-openclaw.homeManagerModules.openclaw ];

        # Render the generated gateway token into the KEY=VALUE shape an
        # environment file has. Only the placeholder lives in the world-readable
        # store copy; agenix decrypts and substitutes at activation, into
        # ~/Library/agenix/templates.
        age.templates."openclaw-env" = {
          dependencies.openclaw-token = config.age.secrets.openclaw-token;
          content = { placeholders, ... }: ''
            OPENCLAW_GATEWAY_TOKEN=${placeholders.openclaw-token}
          '';
        };

        programs.openclaw = {
          enable = true;
          installApp = true;

          # nix-openclaw's plugin-catalog marks `goplaces` defaultEnable = true, so
          # its HM module resolves the plugin via `builtins.getFlake` against the
          # subflake github:openclaw/nix-openclaw-tools?dir=tools/goplaces. That
          # subflake's committed flake.lock pins an input to `path:../..` (a mutable
          # path lock), and Nix refuses to evaluate a locked flake whose lockfile
          # contains a mutable entry -- so the whole config fails to eval with
          # "lock file contains mutable lock". We don't use goplaces (Google Places
          # API), so turn the default off; that drops it from effectivePlugins
          # before getFlake is ever called. Drop this once upstream fixes the lock.
          bundledPlugins.goplaces.enable = false;
          appPackage = inputs.nix-openclaw.packages.${pkgs.stdenv.hostPlatform.system}.openclaw-app;

          # nix-openclaw has no `environmentFiles`; its gateway wrapper instead
          # treats any `environment` value naming an existing file as the file to
          # read the value out of at start, stripping a leading `KEY=` if present.
          # That prefix strip is what lets the agenix template above double as an
          # environment file, so the token reaches the gateway process' env
          # without passing through the store or ~/.config/openclaw/openclaw.json.
          # (A var whose name ends in `_FILE` would be handed the path instead.)
          environment.OPENCLAW_GATEWAY_TOKEN = config.age.templates."openclaw-env".path;

          # `agents.defaults` below routes the primary model through the
          # `claude-cli` agent runtime, which shells out to `claude` -- so the
          # binary has to be on the gateway's PATH. It otherwise would not be:
          # nix-openclaw's launchd agent sets HOME but no PATH, leaving the
          # gateway with launchd's bare /usr/bin:/bin:/usr/sbin:/sbin, so the
          # user's HM profile is not visible to it. runtimePackages is the module's
          # hook for exactly this -- the wrapper prepends these to PATH, without
          # putting them on the interactive user's PATH.
          #
          # Taken from programs.claude-code rather than pkgs.claude-code so the
          # gateway runs the same 2.1.211 binary the dev/opencode aspect pins
          # (nixpkgs is back on 2.1.25). HOME is the real home, so `claude` reuses
          # the already-logged-in credentials under ~/.claude -- no API key.
          runtimePackages = [ config.programs.claude-code.package ];

          # The module's hardcoded default instance omits appDefaults.nixMode and
          # fails to eval on darwin; instances.default routes through the submodule.
          instances.default = {
            # nix-openclaw defaults the `default` instance's log to
            # /tmp/openclaw/openclaw-gateway.log. A nix build sandbox gets there
            # first and leaves /tmp/openclaw owned by _nixbld* at mode 0700, so
            # launchd cannot open the agent's stdout/stderr and parks the job at
            # "spawn scheduled" with active count 0 -- the gateway never execs,
            # and every symptom looks like an auth failure because nothing is
            # listening. Keep the log somewhere only this user owns.
            logPath = "${config.home.homeDirectory}/Library/Logs/openclaw/gateway.log";
          };

          config = {
            gateway = {
              mode = "local";
              bind = "loopback";
              controlUi.enabled = true;

              # SecretRef, not an inline value: the gateway resolves it out of
              # its own environment at runtime, against the var exported by the
              # wrapper above. `provider` has no default in the schema, so it has
              # to be named even though "default" is the only env provider.
              #
              # The wrapper always exports OPENCLAW_GATEWAY_TOKEN, and a present
              # env var outranks the ref (shouldResolveGatewayAuthSecretRef bails
              # early on a token candidate), so startup logs two red [secrets]
              # lines saying this ref is "inactive"/"ignored". That is the healthy
              # path, not a failure -- the ref stays declared as the fallback for
              # when the env var is absent.
              auth = {
                mode = "token";
                token = {
                  source = "env";
                  provider = "default";
                  id = "OPENCLAW_GATEWAY_TOKEN";
                };
              };
            };
            commands.mcp = true;

            # Claude/Gemini via their own logged-in CLIs (CLI-reuse, no API keys).
            #
            # agentRuntime is model-scoped and an absent entry resolves to `auto`
            # rather than inheriting (agent-runtime-architecture.md), so a model
            # only stays pinned to the CLI-reuse path while it has its own entry
            # below -- repointing `primary` alone would silently hand model
            # selection back to auto. opus keeps its entry so switching the
            # default back is a one-line change.
            agents.defaults = {
              model.primary = "anthropic/claude-sonnet-5";
              models = {
                "anthropic/claude-sonnet-5".agentRuntime.id = "claude-cli";
                "anthropic/claude-opus-4-8".agentRuntime.id = "claude-cli";
                "google/gemini-2.5-pro".agentRuntime.id = "google-gemini-cli";
              };
            };

            # Forward the shared programs.mcp.servers registry into OpenClaw's config.
            mcp.servers = lib.mapAttrs (
              _: s:
              lib.filterAttrs (_: v: v != null && v != [ ] && v != { }) {
                inherit (s)
                  command
                  args
                  env
                  url
                  headers
                  ;
              }
            ) config.programs.mcp.servers;
          };
        };
      };
  };
}
