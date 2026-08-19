{
  den,
  inputs,
  rootPath,
  ...
}:
{
  # ZeroClaw (github:zeroclaw-labs/zeroclaw) is a Rust AI-assistant daemon. Its
  # own nix module (`nixosModules.default`) is a NixOS/systemd service — Linux
  # only — so on darwin we run `zeroclaw daemon` under a launchd agent with a
  # Nix-generated ~/.zeroclaw/config.toml instead.
  #
  # A launchd *agent*, not a daemon, and that is load-bearing rather than
  # incidental: two of the MCP servers below reach into GUI apps (Fantastical
  # over XPC, Obsidian over a loopback HTTP server inside its renderer) and are
  # unreachable from outside a logged-in Aqua session.
  #
  # No `inputs.nixpkgs.follows`: ZeroClaw's flake pins its own nixpkgs for the
  # fenix Rust toolchain; forcing follows tends to break the build.
  #
  # But its bundled flake.lock pins a stale fenix (rust 1.93.1) that cannot build
  # the current source (0.8.3 onwards requires rust 1.96.1), so override fenix to
  # a fresh rev via `follows` — verified to clear the toolchain check and compile.
  ff.fenix.url = "github:nix-community/fenix";
  ff.zeroclaw = {
    url = "github:zeroclaw-labs/zeroclaw";
    inputs.fenix.follows = "fenix";
  };

  den.aspects.zeroclaw = {
    includes = [
      # Only for `pkgs.zotero-mcp` and the one `programs.mcp.servers.zotero`
      # entry reused below. The rest of that registry (jujutsu, deepwiki, nixos)
      # is coding tooling and is deliberately NOT granted to this agent — see
      # `lifeServers`.
      den.aspects.mcp-servers
      den.aspects.packages.todoist-mcp
      den.aspects.packages.ms-365-mcp-server
    ];

    homeManager =
      {
        config,
        lib,
        pkgs,
        ...
      }:
      let
        cfg = config.programs.zeroclaw;

        zeroclawPkgs = inputs.zeroclaw.packages.${pkgs.stdenv.hostPlatform.system};
        zeroclawPkg = zeroclawPkgs.default;

        # zerocode is the TUI, and it prints its own warning on --version:
        # "This version must exactly match the running zeroclaw daemon. The TUI
        # and daemon share a wire protocol with no cross-version compatibility
        # guarantee." So it has to come from the same rev as the daemon, which
        # means fixing upstream's packaging rather than reaching for the release
        # binary: `packages.zerocode` reuses the generated feature list meant for
        # the `zeroclaw` crate, and dies with "the package 'zerocode' does not
        # contain these features: acp-bridge, agent-runtime, …". zerocode's own
        # manifest declares `default = []` and one unrelated `sop-authoring`
        # flag, so the correct build is simply no features at all.
        zerocodePkg = zeroclawPkgs.zerocode.overrideAttrs (_: {
          cargoBuildFlags = [
            "-p"
            "zerocode"
            "--no-default-features"
          ];
        });

        # The gateway serves its dashboard out of `gateway.web_dist_dir`, and
        # `zeroclawPkg` (upstream's `packages.default`, a plain `cargo build`)
        # never produces one — hence the gateway's "Web dashboard not available"
        # fallback pointing at `install.sh --source`.
        #
        # This used to reproduce that installer's `cargo xtask web build` here: a
        # second full workspace compile, plus an npm build, plus a hand-maintained
        # FOD hash for `web/package-lock.json`. It stopped working at 0.8.4, where
        # `cargo build -p xtask` dies with a spanless
        # `error[E0463]: can't find crate for 'std'` after every other crate in
        # the workspace has compiled against that same target and toolchain.
        #
        # It is cheaper not to build it at all. The official release tarball ships
        # `web/dist/` already built, so take it from there and throw away the
        # binaries beside it — those still come from the pinned source, because
        # the daemon and its TUI have to match the rev exactly (see zerocodePkg).
        #
        # The dashboard does not: it talks to the gateway over HTTP against a
        # generated OpenAPI client, so a small skew degrades a pane rather than
        # refusing to connect. The URL is pinned to `zeroclawPkg.version`, which
        # bounds that skew to "the input moved to a rev after the tag but before
        # the version bump" — real, but narrow, and it closes the moment
        # upstream cuts the next release. A rev whose version has no release at
        # all fails loudly at fetch time instead.
        webDistTarballs = {
          # `web/dist` is platform-independent JS, but it is only distributed
          # inside the per-platform release archives, so the hash is per-system.
          aarch64-darwin = "sha256-+sIvmvL5QP07t+d960L6x9BS38gSEoEmlHA/fCDTTt8=";
          x86_64-darwin = "sha256-qDD5HCDw0LM4r6pxSWoouWZkn2qwNgHhcl+ucNC6KuU=";
        };

        webDist = pkgs.stdenvNoCC.mkDerivation {
          pname = "zeroclaw-web-dist";
          inherit (zeroclawPkg) version;
          src = pkgs.fetchurl {
            url = "https://github.com/zeroclaw-labs/zeroclaw/releases/download/v${zeroclawPkg.version}/zeroclaw-${pkgs.stdenv.hostPlatform.rust.rustcTarget}.tar.gz";
            hash =
              webDistTarballs.${pkgs.stdenv.hostPlatform.system}
                or (throw "zeroclaw: no release tarball hash for ${pkgs.stdenv.hostPlatform.system}");
          };
          # The archive is flat (`zeroclaw`, `zerocode`, `web/`), so there is no
          # single directory for unpackPhase to descend into.
          sourceRoot = ".";
          installPhase = ''
            runHook preInstall
            cp -r web/dist $out
            runHook postInstall
          '';
          # Static assets; nothing to strip, patch or sign.
          dontFixup = true;
        };

        # Secrets that do not exist yet must not break evaluation, and a server
        # whose credential is missing must not break the daemon: `envsubst
        # -no-unset` fails the whole render on an unresolved placeholder, which
        # would take down Zotero and Fantastical along with it. So each optional
        # credential gates both its own env line and the server that needs it.
        # Create one with `nix run .#secret-edit secrets/<name>.age`, then
        # `nix run .#rekey`, and it switches itself on at the next build.
        secretFile = name: rootPath + "/secrets/${name}.age";
        hasSecret = name: builtins.pathExists (secretFile name);

        home = config.home.homeDirectory;
        zeroclawDir = "${home}/.zeroclaw";

        # The one agent this config defines. Its alias is user-visible: it is the
        # `-a` of `zeroclaw agent -a life`, the directory under agents/, and the
        # name in every approval prompt.
        agent = "life";

        # Where the ms-365 servers keep their MSAL token caches. Outside the
        # store, one pair of files per account, written by `--login`.
        ms365Dir = "${zeroclawDir}/ms365";

        envFiles = map toString cfg.environmentFiles;

        # ── MCP ────────────────────────────────────────────────────────────
        #
        # `programs.mcp.servers` (aspects/dev/llms/mcp-servers.nix) is the
        # *coding* registry: home-manager folds it into a synthesised claude-code
        # plugin, so everything in it appears in every Claude Code and opencode
        # session. Putting Todoist/Outlook/Fantastical there would leak this
        # agent's world into the coding one, which is the exact thing this whole
        # aspect exists to avoid. Hence the separate `programs.zeroclaw.mcpServers`
        # option below, read by nothing else.
        #
        # Zotero is the deliberate crossover: already declared in the shared
        # registry, wanted in both, so it is referenced rather than redeclared.
        lifeServers =
          lib.optionalAttrs (config.programs.mcp.servers ? zotero) {
            inherit (config.programs.mcp.servers) zotero;
          }
          // lib.filterAttrs (_: s: s.enable) cfg.mcpServers;

        # Adapt into ZeroClaw's `[[mcp.servers]]` array-of-tables shape (each
        # entry carries `name`; stdio = command/args/env, http = url/headers).
        toZcServer =
          name: s:
          {
            inherit name;
          }
          # `or` throughout: `lifeServers` mixes entries from this aspect's own
          # submodule with the one home-manager option (zotero), whose freeform
          # submodule does not necessarily define every key.
          // lib.optionalAttrs ((s.tool_timeout_secs or null) != null) {
            tool_timeout_secs = s.tool_timeout_secs;
          }
          // (
            if (s.url or null) != null then
              {
                transport = "http";
                url = s.url;
              }
              // lib.optionalAttrs ((s.headers or { }) != { }) { headers = s.headers; }
            else
              {
                command = s.command;
              }
              // lib.optionalAttrs ((s.args or [ ]) != [ ]) { args = s.args; }
              // lib.optionalAttrs ((s.env or { }) != { }) { env = s.env; }
          );

        # ── Outlook ────────────────────────────────────────────────────────
        #
        # Two accounts, two server entries, two token caches. Sharing a cache
        # would let MSAL hand one server the other's account; separate files plus
        # `--expected-username` turn that into a startup failure rather than a
        # silent cross-mailbox read.
        #
        # `--preset outlook` is mail + calendar + contacts, ~100 tools. Without it
        # the server exposes most of Graph, several hundred.
        #
        # BEFORE FIRST USE: run `outlook-login-work` and `outlook-login-personal`
        # once each. The device-code flow needs a browser; everything after it
        # runs headless off the cached refresh token. The cache is a plain file
        # here rather than a keychain item, because this package is built without
        # keytar — see packages/ms-365-mcp-server/package.nix.
        mkOutlook =
          {
            alias,
            tenant,
            orgMode,
            account,
          }:
          {
            command = lib.getExe pkgs.ms-365-mcp-server;
            args = [
              "--preset"
              "outlook"
            ]
            ++ lib.optional orgMode "--org-mode"
            ++ lib.optionals (account != null) [
              "--expected-username"
              account
            ];
            env = {
              MS365_MCP_TENANT_ID = tenant;
              MS365_MCP_TOKEN_CACHE_PATH = "${ms365Dir}/${alias}.token-cache.json";
              MS365_MCP_SELECTED_ACCOUNT_PATH = "${ms365Dir}/${alias}.selected-account.json";
            };
            tool_timeout_secs = 120;
          };

        outlookServers = {
          outlook_work = mkOutlook {
            alias = "work";
            tenant = "common";
            orgMode = true;
            account = cfg.outlook.workAccount;
          };
          outlook_personal = mkOutlook {
            alias = "personal";
            # "consumers" is the personal-Microsoft-account tenant; "common" would
            # let it bind to a work account too, which is the mix-up the two
            # separate entries exist to prevent.
            tenant = "consumers";
            orgMode = false;
            account = cfg.outlook.personalAccount;
          };
        };

        # ── The rendered config ────────────────────────────────────────────
        zeroclawConfig = {
          # Absolute Nix store path, read literally by the gateway (no `~`
          # expansion) — see webDist above for why this is needed instead of
          # relying on the OS-specific install.sh data-dir auto-detect.
          gateway = {
            web_dist_dir = "${webDist}";
            # These four are already the upstream defaults. Pinned anyway so a
            # future default change shows up as a diff in review rather than as
            # a listener that quietly grew an audience.
            host = "127.0.0.1";
            port = 42617;
            require_pairing = true;
            allow_public_bind = false;
          };

          providers.models = {
            # A `gemini_cli.home` entry (Gemini through the `gemini` CLI's own
            # auth, no key) used to sit here as a fallback and is easy to add
            # back; it was dropped rather than left commented out, because a
            # provider nobody has exercised is not a fallback.
            #
            # Credential comes from the `zeroclaw-env` template below, as a
            # ZEROCLAW_* env override — so it stays out of both the store and
            # ~/.zeroclaw/config.toml. `api_key` takes the `claude setup-token`
            # OAuth token as-is: the provider sniffs the `sk-ant-oat01-` prefix
            # and sends `Authorization: Bearer` + the oauth beta headers instead
            # of `x-api-key` (zeroclaw-providers/src/anthropic.rs).
            anthropic.home.model = "claude-sonnet-4-6";
            anthropic.home.api_key = "$ANTHROPIC_TOKEN";
            # Upstream can auto-populate this from the provider's /models
            # endpoint at setup time, but the daemon re-renders config.toml from
            # the store template at every start, so anything it detects is thrown
            # away on the next restart. Left unset it falls back to 32000 tokens
            # — a quiet, permanent handicap on a 200k model, and the sort of
            # thing that shows up as "the agent keeps forgetting" rather than as
            # an error. Set it here so it survives the re-render.
            anthropic.home.context_window = 200000;
          };

          # Memory retrieval defaults to `hybrid`, which blends BM25 with vector
          # similarity — but there is no embedding provider configured here, so
          # the vector half is silently skipped and recall is keyword-only
          # anyway. Say so, rather than shipping a config that claims a mode it
          # cannot perform. Switch back to "hybrid" alongside a
          # `memory.embedding_provider` if that becomes worth the setup.
          memory.search_mode = "bm25";

          agents.${agent} = {
            model_provider = "anthropic.home";
            risk_profile = agent;
            mcp_bundles = [ agent ]; # secure-by-default: no bundle => no MCP servers
          };

          # No `skill_bundles` entry: skills in the agent's own
          # `<workspace>/skills/` are loaded automatically, which is where
          # home.file plants them (alongside the SOUL.md / IDENTITY.md /
          # AGENTS.md / USER.md that `identity.format = "openclaw"`, the default,
          # reads from the same directory). A `[skill_bundles.life]` block with
          # `directory = "skills"` was tried first and is a trap: it resolves
          # against the install root, not the workspace, so it silently matched
          # nothing while the workspace skills loaded anyway. Verified with
          # `zeroclaw skills list --agent life`, which is the only view that
          # reports what an agent actually loads.

          # Autonomy is per-risk-profile in schema v3; there is no top-level
          # [autonomy] table (that was v2). An empty profile here would mean bare
          # defaults, which is what this daemon last ran on.
          risk_profiles.${agent} = {
            # Not "full": that switch disables approval gates, workspace scoping
            # AND the OS sandbox together, which is three decisions wearing one
            # name. Supervised means reads run and anything that changes the
            # world asks first.
            level = "supervised";
            workspace_only = true;
            # The one carve-out. File tools may leave the workspace for the vault
            # and nowhere else.
            allowed_roots = [ "${home}/Work/Work" ];
            # Blocks regardless of workspace_only, so these stay unreachable even
            # if the workspace posture is later loosened. ~/Library/agenix is
            # where the rekeyed secrets land — including this daemon's own token.
            forbidden_paths = [
              "${home}/code"
              "${home}/dendritic"
              "${home}/.ssh"
              "${home}/.gnupg"
              "${home}/Library/agenix"
              "/etc"
            ];
            require_approval_for_medium_risk = true;
            block_high_risk_commands = true;
            # Defence in depth only — the shell tool is excluded outright below.
            # Note the polarity: a NON-empty list is the strict allowlist, and an
            # empty one means no command constraint at all. Upstream's default is
            # non-empty but coding-shaped (git, cargo, npm, python3, node), which
            # is precisely the reach this agent must not have.
            allowed_commands = [
              "date"
              "ls"
              "cat"
            ];
            shell_env_passthrough = [
              "PATH"
              "HOME"
            ];
            sandbox_enabled = true;
            # "seatbelt" is the darwin backend (sandbox-exec). Named explicitly
            # rather than inferred, so a mis-detect fails visibly.
            sandbox_backend = "seatbelt";
            # `allowed_tools` is deliberately left unset. Empty means "no
            # authorization constraint", and a non-empty list would not help
            # anyway: ZeroClaw auto-admits any tool whose name contains `__`
            # (i.e. every MCP tool) into a non-empty allowlist. `excluded_tools`
            # is an exact-name deny list and is therefore the only real lever.
            #
            # `shell` goes first because it is the one built-in that could walk
            # out of this agent's remit entirely. Everything it actually needs
            # arrives over MCP or the file tools.
            #
            # Destructive MCP tools belong here too, by their prefixed
            # `<server>__<tool>` names — but add them from a live `life` session
            # rather than from guesswork, because a name that matches nothing
            # excludes nothing and looks identical in the config.
            excluded_tools = [ "shell" ];
            # `auto_approve` is left unset on purpose: upstream merges its own
            # read-only defaults (file_read, tool_search, web_fetch, …) in
            # regardless, and those are exactly the "reads run free" half of
            # supervised. Listing mutating tools here is the knob if the approval
            # prompts get tiring.
          };

          channels.cli = true;

          mcp = {
            enabled = true;
            # `--preset outlook` alone is ~100 tools, and there are two of those
            # plus Zotero's large surface. Eager loading would spend most of the
            # context window on tool schemas, so ship names only and let the model
            # pull a schema through `tool_search` when it actually needs one.
            deferred_loading = true;
            servers = lib.mapAttrsToList toZcServer lifeServers;
          };
          mcp_bundles.${agent}.servers = builtins.attrNames lifeServers;
        };

        # Holds the *placeholders*, not the secrets — this copy is world-readable.
        configTemplate = (pkgs.formats.toml { }).generate "zeroclaw-config.toml" zeroclawConfig;

        # launchd's equivalent of upstream's `ExecStartPre` + `EnvironmentFile=`:
        # load the env files, resolve the template into ~/.zeroclaw/config.toml
        # at 0600, then exec the daemon (which inherits the loaded environment,
        # so ZeroClaw's own ZEROCLAW_*/OPENROUTER_API_KEY overrides work too).
        daemon = pkgs.writeShellApplication {
          name = "zeroclaw-daemon";
          runtimeInputs = [ pkgs.envsubst ];
          text = ''
            mkdir -p ${lib.escapeShellArg zeroclawDir}
            # The ms-365 servers were built without keytar, so their MSAL caches
            # and encryption keys are plain files here rather than keychain items.
            mkdir -p ${lib.escapeShellArg ms365Dir}
            chmod 0700 ${lib.escapeShellArg ms365Dir}

            ${lib.optionalString (envFiles != [ ]) ''
              # Sourcing rather than parsing means quoting behaves the way
              # systemd's EnvironmentFile= does. The cost is that the file is
              # evaluated as shell — fine for an operator-owned secret, and the
              # KeepAlive gate means we only get here once it exists.
              set -a
              ${lib.concatMapStringsSep "\n" (f: ''
                if [ ! -r ${lib.escapeShellArg f} ]; then
                  echo "zeroclaw: environment file ${f} is missing or unreadable" >&2
                  exit 1
                fi
                # shellcheck disable=SC1090,SC1091
                . ${lib.escapeShellArg f}
              '') envFiles}
              set +a
            ''}

            # `-no-unset` fails loudly on a placeholder with no matching
            # variable, rather than quietly substituting an empty secret.
            tmp=${lib.escapeShellArg "${zeroclawDir}/.config.toml.tmp"}
            envsubst -no-unset < ${configTemplate} > "$tmp"
            chmod 0600 "$tmp"
            mv -f "$tmp" ${lib.escapeShellArg "${zeroclawDir}/config.toml"}

            exec ${zeroclawPkg}/bin/zeroclaw daemon
          '';
        };

        # ── The front door ─────────────────────────────────────────────────
        #
        # `life` with a message is a one-shot; bare it opens zerocode, which
        # attaches to the running daemon over its local IPC socket rather than
        # starting a second copy of everything.
        lifeCmd = pkgs.writeShellApplication {
          name = "life";
          text = ''
            if [ "$#" -eq 0 ]; then
              exec ${zerocodePkg}/bin/zerocode
            fi
            exec ${zeroclawPkg}/bin/zeroclaw agent -a ${agent} -m "$*"
          '';
        };

        # The daemon rewrites config.toml from the template at every start, which
        # drops the `gateway.paired_tokens` it wrote there itself — so a dashboard
        # pairing does not survive a restart. Rather than persist runtime state
        # through the store, make re-pairing one command.
        lifeDashCmd = pkgs.writeShellApplication {
          name = "life-dash";
          text = ''
            ${zeroclawPkg}/bin/zeroclaw gateway get-paircode --new --port ${toString zeroclawConfig.gateway.port}
            open "http://${zeroclawConfig.gateway.host}:${toString zeroclawConfig.gateway.port}/"
          '';
        };

        # One wrapper per Outlook account for the one-time (and occasional
        # re-auth) device-code login, with that account's cache paths and tenant
        # already set — otherwise it is four env vars to remember correctly at
        # the exact moment the daemon is broken.
        outlookLoginCmd =
          key: s:
          pkgs.writeShellApplication {
            # outlook_work -> outlook-login-work
            name = "outlook-login-${lib.removePrefix "outlook_" key}";
            text = ''
              mkdir -p ${lib.escapeShellArg ms365Dir}
              chmod 0700 ${lib.escapeShellArg ms365Dir}
              ${lib.concatStringsSep "\n" (
                lib.mapAttrsToList (k: v: "export ${k}=${lib.escapeShellArg v}") s.env
              )}
              exec ${s.command} ${lib.escapeShellArgs s.args} --login
            '';
          };
      in
      {
        options.programs.zeroclaw = {
          environmentFiles = lib.mkOption {
            type = lib.types.listOf lib.types.path;
            default = [ ];
            example = lib.literalExpression "[ config.age.secrets.zeroclaw-env.path ]";
            description = ''
              Paths to files of `KEY=VALUE` lines, loaded into the daemon's
              environment before its config is rendered — the launchd counterpart
              of upstream's systemd `EnvironmentFile=`.

              Loaded variables are usable two ways: as `$VAR` / `''${VAR}`
              references inside the config attrset (resolved by `envsubst` at
              daemon start, so the world-readable `/nix/store` copy only ever holds
              the literal placeholder and the resolved `~/.zeroclaw/config.toml` is
              mode `0600`), and as ZeroClaw's own named env overrides, which the
              daemon reads directly from the inherited environment.

              While this is non-empty the launchd agent gates on the files existing
              via `KeepAlive.PathState` — the closest analogue to upstream's
              `ConditionPathExists=`. The agent stays down until they materialise
              (agenix rekeys on activation) and comes back up on its own once they
              do, rather than crash-looping.

              Note these are sourced as shell, so the file is a superset of
              systemd's format: quoting works, but avoid anything you would not
              want executed.
            '';
          };

          mcpServers = lib.mkOption {
            default = { };
            description = ''
              MCP servers granted to the ZeroClaw agent and to nothing else.

              Deliberately *not* `programs.mcp.servers`: that registry is folded
              into the generated claude-code plugin, so anything declared there
              appears in every Claude Code and opencode session. Servers for the
              life agent belong here, where only `[[mcp.servers]]` in
              `~/.zeroclaw/config.toml` reads them.

              Secrets do not go in literally. Write a `$VAR` placeholder in `env`
              or `headers` and add the matching `KEY=VALUE` line to a file in
              `programs.zeroclaw.environmentFiles`; `envsubst` resolves it at
              daemon start into the 0600 config.
            '';
            type = lib.types.attrsOf (
              lib.types.submodule {
                options = {
                  enable = lib.mkOption {
                    type = lib.types.bool;
                    default = true;
                    description = "Whether to grant this server to the agent.";
                  };
                  command = lib.mkOption {
                    type = lib.types.str;
                    default = "";
                    description = "Executable to spawn, for stdio transport.";
                  };
                  args = lib.mkOption {
                    type = lib.types.listOf lib.types.str;
                    default = [ ];
                  };
                  # `attrsOf str`, not home-manager's `str or { file = ...; }`:
                  # ZeroClaw's schema is `HashMap<String, String>`, so the
                  # file-reference form would serialise as a nested TOML table
                  # that the daemon silently fails to resolve — the bug the old
                  # `env = s.env` passthrough was one secret away from hitting.
                  # The narrower type turns it into a type error at eval instead.
                  env = lib.mkOption {
                    type = lib.types.attrsOf lib.types.str;
                    default = { };
                    description = ''
                      Environment for a stdio server. Values are plain strings;
                      for a secret use a `$VAR` placeholder (see the option
                      description above), never a path to the secret.
                    '';
                  };
                  url = lib.mkOption {
                    type = lib.types.nullOr lib.types.str;
                    default = null;
                    description = "Endpoint for an http-transport server. Mutually exclusive with `command`.";
                  };
                  headers = lib.mkOption {
                    type = lib.types.attrsOf lib.types.str;
                    default = { };
                  };
                  tool_timeout_secs = lib.mkOption {
                    type = lib.types.nullOr lib.types.ints.positive;
                    default = null;
                    description = "Per-call timeout. Worth setting for servers that reach into a GUI app.";
                  };
                };
              }
            );
          };

          outlook = {
            workAccount = lib.mkOption {
              type = lib.types.nullOr lib.types.str;
              default = null;
              example = "ivy@student.unimelb.edu.au";
              description = ''
                UPN of the work/university Microsoft 365 account, used to pin the
                `outlook_work` server to one identity (`--expected-username`).
                Unset means no pin: the server will use whatever account happens
                to be in its token cache.
              '';
            };
            personalAccount = lib.mkOption {
              type = lib.types.nullOr lib.types.str;
              default = null;
              example = "ivy@outlook.com";
              description = "UPN of the personal Outlook.com account, pinning `outlook_personal`.";
            };
          };
        };

        config = {
          assertions = [
            {
              assertion = lib.all (s: s.url == null -> s.command != "") (builtins.attrValues cfg.mcpServers);
              message = "programs.zeroclaw.mcpServers: every server needs either `url` (http) or `command` (stdio).";
            }
          ];

          # The Claude subscription token from `claude setup-token`, plus the two
          # static credentials the life servers need. Declared raw rather than
          # through the `secrets` den class to stay consistent with how this file
          # already reaches for claude_token.
          age.secrets = {
            claude_token.rekeyFile = secretFile "claude_token";
          }
          // lib.optionalAttrs (hasSecret "todoist_key") {
            todoist_key.rekeyFile = secretFile "todoist_key";
          }
          // lib.optionalAttrs (hasSecret "obsidian_rest_key") {
            obsidian_rest_key.rekeyFile = secretFile "obsidian_rest_key";
          };

          # Render the raw tokens into the KEY=VALUE shape `environmentFiles`
          # expects. Only the placeholder lives in the store copy; agenix
          # decrypts and substitutes at activation, into ~/Library/agenix/
          # templates. It reads each secret through `$(...)`, which strips the
          # trailing newline, so a token file saved by an editor still yields a
          # clean single-line assignment.
          age.templates."zeroclaw-env" =
            let
              # env var -> secret name. Every entry becomes one KEY=VALUE line and
              # one `$VAR` that `envsubst -no-unset` must be able to resolve, so
              # the set has to track exactly which secrets actually exist.
              vars = {
                ANTHROPIC_TOKEN = "claude_token";
              }
              // lib.optionalAttrs (hasSecret "todoist_key") { TODOIST_API_KEY = "todoist_key"; }
              // lib.optionalAttrs (hasSecret "obsidian_rest_key") {
                OBSIDIAN_API_KEY = "obsidian_rest_key";
              };
            in
            {
              dependencies = lib.mapAttrs' (
                _: secret: lib.nameValuePair secret config.age.secrets.${secret}
              ) vars;
              content =
                { placeholders, ... }:
                lib.concatStrings (lib.mapAttrsToList (var: secret: "${var}=${placeholders.${secret}}\n") vars);
            };

          # ZeroClaw applies ZEROCLAW_<dotted__path> overrides after loading the
          # config and masks them back out before any save, so the token reaches
          # neither the store nor ~/.zeroclaw/config.toml. Note it hard-errors on
          # a path that doesn't resolve — this one is verified against the 0.8.x
          # schema, but it needs rechecking if the provider alias is renamed.
          programs.zeroclaw.environmentFiles = [ config.age.templates."zeroclaw-env".path ];

          programs.zeroclaw.mcpServers = {
            # Obsidian's "Local REST API" plugin (>= 5.0) serves MCP from inside
            # Obsidian itself, so there is no subprocess — and no vault when
            # Obsidian is closed. Plain HTTP on 27123 rather than 27124, whose
            # TLS is self-signed and would need a trust dance in a daemon.
            #
            # MANUAL PREREQUISITE: install the plugin into ~/Work/Work, enable
            # "Enable HTTP server", and put its API key in secrets/obsidian_rest_key.age.
            # Until then the server is simply unreachable, which ZeroClaw reports
            # as a non-fatal warning at session start.
            #
            # The plugin serves whichever vault is *currently open*. With three
            # vaults registered, opening a different one silently repoints the
            # agent — TOOLS.md tells it to say so if results look wrong.
            obsidian = {
              enable = hasSecret "obsidian_rest_key";
              url = "http://127.0.0.1:27123/mcp/";
              headers.Authorization = "Bearer $OBSIDIAN_API_KEY";
              tool_timeout_secs = 60;
            };

            # Fantastical's in-app MCP server (>= 4.1.17). The helper is a thin
            # XPC client pinned by code signature to
            # 85C27NK92C.com.flexibits.fantastical2.mac.helper, so it needs
            # Fantastical.app running and cannot be reached from outside the Aqua
            # session. Path is outside the store by nature — Fantastical is a
            # /Applications app, not nix-managed — so an app update that moves the
            # helper breaks this server (and only this server).
            fantastical = {
              command = "/Applications/Fantastical.app/Contents/Helpers/FantasticalMCP.app/Contents/MacOS/FantasticalMCP";
              tool_timeout_secs = 60;
            };

            # Note the variable is TODOIST_API_KEY, not _API_TOKEN.
            todoist = {
              enable = hasSecret "todoist_key";
              command = lib.getExe pkgs.todoist-mcp;
              env.TODOIST_API_KEY = "$TODOIST_API_KEY";
            };
          }
          // outlookServers;

          # The persona. These four are configuration, not memory: they come from
          # the store read-only, and AGENTS.md tells the agent so, because the
          # stock upstream AGENTS.md invites it to edit them as it learns.
          # IDENTITY.md, MEMORY.md, HEARTBEAT.md and memory/ are deliberately left
          # unmanaged — those are the agent's own writable state.
          home.file =
            lib.listToAttrs (
              map
                (f: {
                  name = ".zeroclaw/agents/${agent}/workspace/${f}";
                  value.source = ./persona/${f};
                })
                [
                  "SOUL.md"
                  "USER.md"
                  "AGENTS.md"
                  "TOOLS.md"
                ]
            )
            // {
              # Auto-loaded from the workspace; see the agents.${agent} block.
              ".zeroclaw/agents/${agent}/workspace/skills".source = ./skills;

              # ~/.zeroclaw/config.toml is written by the daemon at start (it is the
              # resolved, 0600 copy), so it deliberately isn't a home.file symlink.
              # This marker just guarantees the directory exists before launchd
              # tries to open the log paths below.
              ".zeroclaw/.keep".text = "";
            };

          home.packages = [
            # The CLI itself — `zeroclaw status`, `zeroclaw estop kill-all`,
            # `zeroclaw gateway`. Previously only the launchd wrapper referenced
            # it, so none of that was reachable by hand.
            zeroclawPkg
            # The TUI that attaches to the running daemon. This is the front door.
            zerocodePkg
            lifeCmd
            lifeDashCmd
          ]
          ++ lib.mapAttrsToList outlookLoginCmd outlookServers;

          # launchd agent: runs the daemon at login, restarts it if it dies. Needs
          # the `gemini`/`claude` CLIs on PATH (present in the HM profile).
          launchd.agents.zeroclaw = {
            enable = true;
            config = {
              ProgramArguments = [ (lib.getExe daemon) ];
              RunAtLoad = true;
              KeepAlive = if envFiles == [ ] then true else { PathState = lib.genAttrs envFiles (_: true); };
              ProcessType = "Background";
              StandardOutPath = "${zeroclawDir}/zeroclaw.log";
              StandardErrorPath = "${zeroclawDir}/zeroclaw.err.log";
              EnvironmentVariables = {
                HOME = config.home.homeDirectory;
                PATH = "${config.home.profileDirectory}/bin:/usr/bin:/bin:/usr/sbin:/sbin";
              };
            };
          };
        };
      };
  };
}
