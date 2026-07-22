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
  # No `inputs.nixpkgs.follows`: ZeroClaw's flake pins its own nixpkgs for the
  # fenix Rust toolchain; forcing follows tends to break the build.
  #
  # But its bundled flake.lock pins a stale fenix (rust 1.93.1) that cannot build
  # the current 0.8.3 source (which requires rust 1.96.1), so override fenix to a
  # fresh rev via `follows` — verified to clear the toolchain check and compile.
  flake-file.inputs.fenix.url = "github:nix-community/fenix";
  flake-file.inputs.zeroclaw = {
    url = "github:zeroclaw-labs/zeroclaw";
    inputs.fenix.follows = "fenix";
  };

  den.aspects.zeroclaw = {
    # Shared MCP registry (programs.mcp.servers) — same source opencode/openclaw
    # use; piped into ZeroClaw's config.toml `[[mcp.servers]]` below.
    includes = [ den.aspects.mcp-servers ];

    homeManager =
      {
        config,
        lib,
        pkgs,
        ...
      }:
      let
        cfg = config.programs.zeroclaw;

        zeroclawPkg = inputs.zeroclaw.packages.${pkgs.stdenv.hostPlatform.system}.default;

        # Same fenix toolchain used to build zeroclawPkg above (see the
        # flake-file.inputs.fenix comment: upstream's bundled fenix lock is
        # too stale for 0.8.3, so it's overridden to a fresh rev via follows).
        rustToolchain = inputs.fenix.packages.${pkgs.stdenv.hostPlatform.system}.stable.withComponents [
          "cargo"
          "clippy"
          "rust-src"
          "rustc"
          "rustfmt"
        ];

        # `zeroclawPkg` (upstream's `packages.default`) is a pure `cargo
        # build` — it never produces the web dashboard (`web/dist`), which is
        # why the gateway falls back to "Web dashboard not available" and
        # points at `install.sh --source`. That installer's dashboard build
        # is really `cargo xtask web build`: compile xtask's `web` binary
        # (it links `zeroclaw-gateway` to emit the OpenAPI spec plus
        # generated TS enum/description modules that `web/src/lib/sops.ts`
        # imports — without them `tsc`/`vite` fail), then run it, which
        # chains `npx openapi-typescript` and `npm run build` (tsc -b + vite).
        # Reproduced here so `gateway.web_dist_dir` below always points at a
        # dashboard build that matches the pinned zeroclaw rev.
        webDist =
          (pkgs.makeRustPlatform {
            cargo = rustToolchain;
            rustc = rustToolchain;
          }).buildRustPackage
            {
              pname = "zeroclaw-web-dist";
              version = "0.8.3";
              src = inputs.zeroclaw;
              # Same lockfile/patched-hash pair as zeroclawPkg — bump both
              # together when the zeroclaw input rev changes.
              cargoLock = {
                lockFile = "${inputs.zeroclaw}/Cargo.lock";
                outputHashes = builtins.fromJSON (builtins.readFile "${inputs.zeroclaw}/nix/hashes.json");
              };
              cargoBuildFlags = [
                "-p"
                "xtask"
                "--bin"
                "web"
              ];
              doCheck = false;
              buildInputs = [ pkgs.stdenv.cc.cc ];

              nativeBuildInputs = [
                pkgs.nodejs_24
                pkgs.npmHooks.npmConfigHook
              ];
              npmRoot = "web";
              # `web/package-lock.json`'s FOD — needs re-hashing (set
              # lib.fakeHash, rebuild, copy the "got:" value) whenever the
              # zeroclaw input rev changes its web/ lockfile.
              npmDeps = pkgs.fetchNpmDeps {
                name = "zeroclaw-web-npm-deps";
                src = "${inputs.zeroclaw}/web";
                hash = "sha256-SKltlDJm39ZzVaEt1bbnoiXy+wlbq+fC3bO4mW5V15o=";
              };

              # The `web` binary bakes its repo root in via Rust's
              # `env!("CARGO_MANIFEST_DIR")`, resolved at *this* compile, so
              # it has to run inside this same build rather than as a
              # separately installed binary (that path won't exist later).
              # cargoInstallHook's usual $out/bin install is skipped for the
              # same reason — only web/dist is wanted here.
              installPhase = ''
                runHook preInstall
                ./target/${pkgs.stdenv.hostPlatform.rust.rustcTarget}/release/web build
                cp -r web/dist $out
                runHook postInstall
              '';
            };

        zeroclawDir = "${config.home.homeDirectory}/.zeroclaw";

        envFiles = map toString cfg.environmentFiles;

        # Adapt the HM-core `programs.mcp.servers` attrset into ZeroClaw's
        # `[[mcp.servers]]` array-of-tables shape (each entry carries `name`;
        # stdio = command/args/env, http = url/headers).
        mcpServers = config.programs.mcp.servers;
        mcpNames = builtins.attrNames mcpServers;
        toZcServer =
          name: s:
          {
            inherit name;
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

        zeroclawConfig = {
          # Absolute Nix store path, read literally by the gateway (no `~`
          # expansion) — see webDist above for why this is needed instead of
          # relying on the OS-specific install.sh data-dir auto-detect.
          gateway.web_dist_dir = "${webDist}";

          providers.models = {
            # Gemini via the `gemini` CLI's own auth — pure CLI reuse, no key.
            #            gemini_cli.home.model = "gemini-2.5-pro";
            # Credential comes from the `zeroclaw-env` template below, as a
            # ZEROCLAW_* env override — so it stays out of both the store and
            # ~/.zeroclaw/config.toml. `api_key` takes the `claude setup-token`
            # OAuth token as-is: the provider sniffs the `sk-ant-oat01-` prefix
            # and sends `Authorization: Bearer` + the oauth beta headers instead
            # of `x-api-key` (zeroclaw-providers/src/anthropic.rs).
            anthropic.home.model = "claude-sonnet-4-6";
            anthropic.home.api_key = "$ANTHROPIC_TOKEN";
          };

          agents.assistant = {
            # gemini_cli.home stays configured above as a fallback to switch back to.
            model_provider = "anthropic.home";
            risk_profile = "assistant";
            mcp_bundles = [ "all" ]; # secure-by-default: no bundle => no MCP servers
          };
          risk_profiles.assistant = { };

          mcp = {
            enabled = true;
            servers = lib.mapAttrsToList toZcServer mcpServers;
          };
          mcp_bundles.all.servers = mcpNames;
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
      in
      {
        options.programs.zeroclaw.environmentFiles = lib.mkOption {
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

        config = {
          # The Claude subscription token from `claude setup-token`.
          age.secrets.claude_token.rekeyFile = rootPath + "/secrets/claude_token.age";

          # Render the raw token into the KEY=VALUE shape `environmentFiles`
          # expects. Only the placeholder lives in the store copy; agenix
          # decrypts and substitutes at activation, into ~/Library/agenix/
          # templates. It reads the secret through `$(...)`, which strips the
          # trailing newline, so a token file saved by an editor still yields a
          # clean single-line assignment.
          age.templates."zeroclaw-env" = {
            dependencies.claude_token = config.age.secrets.claude_token;
            content = { placeholders, ... }: ''
              ANTHROPIC_TOKEN=${placeholders.claude_token}
            '';
          };

          # ZeroClaw applies ZEROCLAW_<dotted__path> overrides after loading the
          # config and masks them back out before any save, so the token reaches
          # neither the store nor ~/.zeroclaw/config.toml. Note it hard-errors on
          # a path that doesn't resolve — this one is verified against 0.8.3's
          # schema, but it needs rechecking if the provider alias is renamed.
          programs.zeroclaw.environmentFiles = [ config.age.templates."zeroclaw-env".path ];

          # ~/.zeroclaw/config.toml is written by the daemon at start (it is the
          # resolved, 0600 copy), so it deliberately isn't a home.file symlink.
          # This marker just guarantees the directory exists before launchd tries
          # to open the log paths below.
          home.file.".zeroclaw/.keep".text = "";

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
