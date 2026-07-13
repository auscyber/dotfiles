{ lib, den, ... }:
let
  address = [
    "127.0.0.1"
    27631
  ];

  lspmux = {
    listen = address;
    instance_timeout = 300; # after 5 minutes

    # time in seconds how long to wait between the gc task checks for disconnected
    # clients and possibly starts a timeout task. the value must be at least 1.
    gc_interval = 10; # every 10 seconds
    connect = address; # same as `listen`
    log_filters = "info";

    # glob filters over the env vars `lspmux client` forwards to the server.
    # `PATH` matters most: it is how a server finds its own toolchain (ghc for
    # hls, the project's node/python, ...), and it is part of the instance key,
    # so two projects with different toolchains get separate server instances.
    pass_environment = [ "*" ];
  };

  # The servers we multiplex, keyed by their nixvim/lspconfig name. Kept in its own
  # file so the `lspmux-server-names` check can import it without the overlay.
  servers = import ./_lspmux-servers.nix;

in
{

  den.aspects.lspmux =
    { user, ... }:
    {
      includes = [ den.aspects.packages.lspmux ];

      overlays = {
        lspmux-wrap = [
          (final: prev: {

            # Turn a language server into a drop-in replacement for itself: the
            # result is the original package with `bin/<exe>` swapped for a shim
            # that runs the server under `lspmux client`.
            #
            # The shim *resolves the server at runtime* rather than hardcoding one
            # store path: it walks $PATH for `exe` and uses the first hit, falling
            # back to the pinned package only if the project provides nothing. So a
            # repo whose devshell pins its own rust-analyzer gets *that* binary --
            # still multiplexed -- instead of silently bypassing lspmux or being
            # forced onto our version. Note $PATH here is the *editor's* env, which
            # is what direnv/devshell manipulation lands in.
            #
            # The self-check in the loop is load-bearing: for `dynamicCmd` servers the
            # shim is on $PATH under the server's own name, so without it the lookup
            # would find the shim and exec itself forever.
            wrapLspMux =
              {
                package,
                exe ? baseNameOf (lib.getExe package),
                lspconfig ? exe,
                args ? [ ],
                dynamicCmd ? false,
              }:
              let
                fallback = "${package}/bin/${exe}";

                # `--` is load-bearing: SERVER_ARGS is a bare positional Vec<String> in
                # lspmux's clap parser, so without it any caller-supplied flag
                # (`--stdio`, `--lsp`, jdtls' `-configuration ...`) is parsed as an
                # lspmux flag and the client dies. Everything after `--` reaches the
                # real server verbatim, whitespace and leading dashes intact.
                shim = prev.writeShellScript "${exe}-lspmux" ''
                  set -u

                  self=$(${prev.coreutils}/bin/realpath "$0" 2>/dev/null || echo "$0")
                  server=""

                  IFS=: read -ra dirs <<< "''${PATH:-}"
                  for dir in "''${dirs[@]}"; do
                    candidate="$dir/${exe}"
                    [ -x "$candidate" ] || continue

                    resolved=$(${prev.coreutils}/bin/realpath "$candidate" 2>/dev/null || echo "$candidate")

                    # Skip *any* lspmux shim, not just this one. A stale generation of
                    # the same wrapper elsewhere on $PATH resolves to a different store
                    # path, and exec'ing it would double-mux (or loop). Shims are the
                    # only things named `*-lspmux` in the store.
                    case "$resolved" in
                      *-lspmux) continue ;;
                    esac
                    [ "$resolved" = "$self" ] && continue

                    server="$resolved"
                    break
                  done

                  [ -n "$server" ] || server=${lib.escapeShellArg fallback}

                  exec ${lib.getExe final.lspmux} client --server-path "$server" -- "$@"
                '';
              in
              prev.symlinkJoin {
                name = "${lib.getName package}-lspmux";
                paths = [ package ];

                postBuild = ''
                  if [ ! -x ${lib.escapeShellArg fallback} ]; then
                    echo "wrapLspMux: ${lib.getName package} has no executable bin/${exe}" >&2
                    exit 1
                  fi

                  rm -f "$out/bin/${exe}"
                  ln -s ${shim} "$out/bin/${exe}"
                '';

                passthru = (package.passthru or { }) // {
                  unwrapped = package;
                  lspmux = {
                    inherit
                      exe
                      lspconfig
                      args
                      dynamicCmd
                      ;
                  };
                };

                meta = (package.meta or { }) // {
                  mainProgram = exe;
                };
              };

            # The multiplexed twin of each server, reachable under every name you might
            # plausibly reach for -- all three keys are the same derivation:
            #
            #   pkgs.lspmuxed.rust_analyzer          lspconfig/nixvim server name
            #   pkgs.lspmuxed."rust-analyzer"        the binary it is spawned as
            #   pkgs.lspmuxed.pyright                the nixpkgs package name
            #
            # (pyright is the case where all three differ: server `pyright`, binary
            # `pyright-langserver`, package `pyright`. clangd's package is clang-tools.)
            #
            # Deliberately *not* an override of the top-level attrs: clobbering
            # pkgs.rust-analyzer would multiplex it inside unrelated build inputs and
            # mass-rebuild everything that depends on it.
            lspmuxed =
              let
                wrapped = map final.wrapLspMux (servers final);
                keyBy = key: lib.listToAttrs (map (server: lib.nameValuePair (key server) server) wrapped);
              in
              # The lspconfig key is merged last: where an alias collides with it the
              # derivation is the same anyway (gopls, clangd, zls, ...), but this keeps
              # the canonical name authoritative rather than dependent on merge order.
              keyBy (server: lib.getName server.passthru.unwrapped)
              // keyBy (server: server.passthru.lspmux.exe)
              // keyBy (server: server.passthru.lspmux.lspconfig);
          })
        ];
      };

      # Point nvim's language servers at the shims. This lives here rather than in the
      # nixvim aspect so that lspmux stays a single opt-in: include this aspect and the
      # editors get multiplexed, drop it and they go back to spawning servers directly.
      nvim =
        { pkgs, ... }:
        {
          # Keyed off each entry's `lspconfig` name -- *not* by mapping over
          # `pkgs.lspmuxed`, which also carries binary- and package-name aliases. Those
          # are not nixvim server names, and `plugins.lsp.servers.<alias>` would be an
          # unknown option.
          plugins.lsp.servers = lib.listToAttrs (
            map (
              spec:
              let
                server = pkgs.lspmuxed.${spec.lspconfig};
              in
              lib.nameValuePair spec.lspconfig (
                if spec.dynamicCmd or false then
                  # lspconfig builds this server's cmd in a lua function at runtime, and we
                  # cannot replace it without losing that logic (jdtls derives `-data
                  # <workspace>`, ts_ls prefers a project-local node_modules server). So
                  # shadow the binary name on $PATH instead: the function spawns it by bare
                  # name, lands in the shim, and its computed args are forwarded verbatim.
                  { package = server; }
                else
                  {
                    # Spawn the shim by path; it resolves the real server off $PATH at
                    # runtime. `package = null` keeps nixvim from prefixing the pinned
                    # server onto nvim's $PATH, where it would beat the project's own copy.
                    package = null;
                    cmd = [ (lib.getExe server) ] ++ (spec.args or [ ]);
                  }
              )
            ) (servers pkgs)
          );
        };

      homeManager =
        { pkgs, ... }:
        {
          home.packages = [
            pkgs.lspmux
          ];
        };

      hmLinux =
        { config, pkgs, ... }:
        {
          home.file.".config/lspmux/config.toml".source = pkgs.writers.writeTOML "lspmux.toml" lspmux;

          systemd.user.services.lspmux = {
            description = "lspmux service";
            after = [ "network.target" ];
            wantedBy = [ "default.target" ];
            serviceConfig = {
              ExecStart = "${pkgs.lspmux}/bin/lspmux server";
              Restart = "always";
              RestartSec = 5;
              StandardOutput = "append:/tmp/lspmux.log";
              StandardError = "append:/tmp/lspmux.log";
            };
          };
        };

      hmDarwin =
        { config, pkgs, ... }:
        {

          home.file."Library/Application Support/lspmux/config.toml".source =
            pkgs.writers.writeTOML "lspmux.toml" lspmux;

          launchd.agents.lspmux = {
            enable = true;
            config = {

              ProgramArguments = [
                "${pkgs.lspmux}/bin/lspmux"
                "server"
              ];

              EnvironmentVariables = {
                PATH = lib.concatStringsSep ":" [
                  "/usr/bin"
                  "/bin"
                  "/usr/sbin"
                  "/sbin"
                ];
              };

              StandardOutPath = "/tmp/lspmux.log";
              StandardErrorPath = "/tmp/lspmux.log";

              KeepAlive = true;
              RunAtLoad = true;

              LimitLoadToSessionType = [
                "Aqua"
                "Background"
                "LoginWindow"
                "StandardIO"
                "System"
              ];
            };
          };
        };

    };
}
