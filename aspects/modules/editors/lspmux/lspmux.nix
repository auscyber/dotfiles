{
  lib,
  den,
  ...
}:
let
  # One multiplexed language server. This is the shape the `lsp-servers` den class
  # is collected into, and the element type of the `lsp.servers` (homeManager) /
  # `lspmux.servers` (nvim) registries the class is routed into.
  lspSubModule = lib.types.submodule (
    { config, ... }:
    {
      options = {
        package = lib.mkOption {
          type = lib.types.nullOr lib.types.package;
          default = null;
          description = "The pinned language server package. Only a fallback: the shim prefers whatever `exe` the current project puts on $PATH.";
        };
        exe = lib.mkOption {
          type = lib.types.str;
          default = baseNameOf (lib.getExe config.package);
          description = "The binary the server is spawned as / looked up on $PATH.";
        };
        args = lib.mkOption {
          type = lib.types.listOf lib.types.str;
          default = [ ];
          description = "The tail of lspconfig's `cmd`, passed by editors that spawn the shim by absolute path.";
        };
        dynamicCmd = lib.mkOption {
          type = lib.types.bool;
          default = false;
          description = "lspconfig builds this server's `cmd` in a lua function at runtime; the shim is name-shadowed on $PATH instead.";
        };
        nvim = lib.mkOption {
          type = lib.types.bool;
          default = true;
          description = ''
            Whether the lspmux `nvim` body should wire this into nixvim's `lsp.servers`.
            Set false for a server nvim drives another way (rust -> rustaceanvim,
            idris -> plugins.idris2) while still wanting the multiplexed twin.
          '';
        };

        lspconfig = lib.mkOption {
          type = lib.types.str;
          default = config.exe;
          description = "The nvim-lspconfig server name. Normally the registry key already is this.";
        };
        generic_name = lib.mkOption {
          type = lib.types.str;
          default = config.exe;
          description = "Fallback editor-side name for servers that don't override zed/opencode.";
        };

        # zed's own server name for this language, or null to not surface it in
        # zed. zed keys `lsp.<name>` by its own vocabulary (`nix`, not `nil_ls`),
        # so it cannot be derived from the lspconfig/exe/package keys.
        zed = lib.mkOption {
          type = lib.types.nullOr lib.types.str;
          default = config.generic_name;
          description = "zed's own server name for this language, or null to not surface it in zed.";
        };
        # opencode's own server id for this language. Defaults to `exe`; only
        # surfaced in opencode when it names one of opencode's *built-in* ids
        # (`rust`, `typescript`, `gopls`, `pyright`, `clangd`, `zls`, `lua-ls`,
        # `nixd`, `haskell-language-server`, `ocaml-lsp`, `kotlin-ls`, ...) --
        # overriding a built-in's `command` needs no `extensions`. Set to a
        # built-in id where `exe` differs (nil -> `nixd`, hls -> `haskell-language-server`,
        # pyright-langserver -> `pyright`), or null to opt out.
        opencode = lib.mkOption {
          type = lib.types.nullOr lib.types.str;
          default = config.exe;
          description = "opencode's server id to override with the shim (surfaced only when it is an opencode built-in), or null to opt out.";
        };
        server_config = lib.mkOption {
          type = lib.types.attrs;
          default = { };
          description = "Extra per-editor server config merged into the opencode entry (e.g. `extensions`, `initialization`, `env`).";
        };
      };
    }
  );
  inherit (den.lib.policy) route;

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
    pass_environment = [
      "*"
      "!SHLVL"
      "!GPG_TTY"
      "!STARSHIP_SESSION_KEY"
      "!DIRENV_DIFF"
      "!DISPLAY"
      "!TMPDIR"
      "!DIRENV_WATCHES"
      "!NVIM"
    ];
  };

  # opencode's built-in LSP server ids. Only these can have their `command`
  # overridden with the shim without also supplying `extensions`, so the opencode
  # feed below surfaces a server only when its `opencode` name is one of these.
  opencodeBuiltins = [
    "typescript"
    "gopls"
    "pyright"
    "rust"
    "clangd"
    "zls"
    "lua-ls"
    "nixd"
    "haskell-language-server"
    "ocaml-lsp"
    "kotlin-ls"
    "csharp"
    "bash"
  ];

  # Build the `lspmuxed` attrset from a resolved `{ <lspconfig> = spec; }` set: the
  # multiplexed twin of each server, reachable under every name you might plausibly
  # reach for -- all three keys are the same derivation:
  #
  #   pkgs.lspmuxed.rust_analyzer          lspconfig/nixvim server name
  #   pkgs.lspmuxed."rust-analyzer"        the binary it is spawned as
  #   pkgs.lspmuxed.pyright                the nixpkgs package name
  #
  # (pyright is the case where all three differ: server `pyright`, binary
  # `pyright-langserver`, package `pyright`. clangd's package is clang-tools.)
  buildLspmuxed =
    final: serversAttr:
    let
      wrapped = lib.mapAttrsToList (
        lspconfig: spec: final.wrapLspMux (spec // { inherit lspconfig; })
      ) serversAttr;
      keyBy = key: lib.listToAttrs (map (server: lib.nameValuePair (key server) server) wrapped);
    in
    # The lspconfig key is merged last: where an alias collides with it the
    # derivation is the same anyway (gopls, clangd, zls, ...), but this keeps the
    # canonical name authoritative rather than dependent on merge order.
    keyBy (server: lib.getName server.passthru.unwrapped)
    // keyBy (server: server.passthru.lspmux.exe)
    // keyBy (server: server.passthru.lspmux.lspconfig);

  # Reshape the collected `lsp-servers` class content (one or more
  # `{ pkgs, ... }: { <lspconfig> = spec; }` bodies) into a single overlay that
  # extends `pkgs.lspmuxed`. The body is re-evaluated with `pkgs` bound to the
  # overlay's `prev` (super), so the fallback package tracks the project's own
  # package set -- and there is no recursion, since `prev` does not depend on
  # `lspmuxed`.
  serversOverlayFromSource = srcMod: {
    lsp-servers-wrap =
      final: prev:
      let
        serversAttr =
          (lib.evalModules {
            specialArgs = {
              pkgs = prev;
            };
            modules = [
              { config._module.freeformType = lib.types.lazyAttrsOf lib.types.raw; }
              srcMod
            ];
          }).config;
      in
      {
        lspmuxed = (prev.lspmuxed or { }) // buildLspmuxed final serversAttr;
      };
  };

  # Forward the `lsp-servers` class into the overlay collection. aspects/utils/
  # overlays.nix feeds `pkgs` from TWO places -- the perSystem `_collectedOverlays`
  # (checks/packages/devshells) and each host/user's `_overlays` (the config that
  # actually builds `nixpkgs.overlays`) -- so we mirror both targets. `evalConfig =
  # true` puts each forward on den's complex-forward path so `mapModule` (the
  # reshape) runs. NB: forwarding into the *synthetic* `overlays` class does NOT
  # work -- overlays.nix re-collects that class with its own route and never sees
  # forwarded content -- so we deliver straight into the routed option paths.
  lspServersToOverlays = den.batteries.forward {
    each = [
      {
        class = "flake-parts";
        path = [ "_collectedOverlays" ];
      }
      {
        class = "nixos";
        path = [ "_overlays" ];
      }
      {
        class = "darwin";
        path = [ "_overlays" ];
      }
      {
        class = "homeManager";
        path = [ "_overlays" ];
      }
    ];
    fromClass = _: "lsp-servers";
    intoClass = item: item.class;
    intoPath = item: item.path;
    evalConfig = true;
    mapModule = _item: srcMod: serversOverlayFromSource srcMod;
  };
in
{
  # The language servers we multiplex are declared as a first-class den `class`
  # rather than a hand-imported list: any aspect can co-locate its own server with
  # its language config via `lsp-servers.<lspconfig> = { ... }`, and den's
  # transform pipeline collects every entry. The registry is routed four ways:
  #
  #   - forwarded into the `overlays` class            -> builds `pkgs.lspmuxed` (zed, idris)
  #   - routed into homeManager `lsp.servers`          -> opencode's `settings.lsp`
  #   - routed into the nvim module `lspmux.servers`   -> nixvim's `lsp.servers`
  #   - routed into a perSystem flake-parts option     -> the server-names check
  #
  # The class body is a `{ pkgs, ... }:` configurator; `pkgs` is injected by each
  # collector. Each entry is an `lspSubModule` (see the top of this file), keyed by
  # its nvim-lspconfig server name -- `package`/`exe`/`args`/`dynamicCmd` plus the
  # per-editor `nvim`/`zed`/`opencode` names.
  #
  # `exe`/`args` are hand-written but not trusted: the `lspmux-server-names` flake
  # check diffs every entry against nvim-lspconfig's own `lsp/<lspconfig>.lua`.
  den.classes.lsp-servers = { };

  # Collect the class subtree into a perSystem option the check reads (mirrors
  # `overlays` -> `_collectedOverlays` in aspects/utils/overlays.nix).
  den.policies.lsp-servers-to-flake-parts = _: [
    (route {
      fromClass = "lsp-servers";
      intoClass = "flake-parts";
      path = [ "_lspServers" ];
      collectSubtree = true;
    })
  ];
  den.schema.flake-parts.includes = [ den.policies.lsp-servers-to-flake-parts ];

  # Route the same class into the two module systems that consume it as a typed
  # registry instead of via the `pkgs.lspmuxed` overlay: home-manager's `lsp.servers`
  # (read by opencode) and nixvim's `lspmux.servers` twin (read by nvim). The class
  # bodies are re-evaluated with each target's own `pkgs`, so the fallback packages
  # track that scope. Included on the aspect below so they run wherever lspmux is.
  den.policies.lsp-servers-to-homeManager = _: [
    (route {
      fromClass = "lsp-servers";
      intoClass = "homeManager";
      path = [
        "lsp"
        "servers"
      ];
      collectSubtree = true;
    })
  ];
  den.policies.lsp-servers-to-nvim = _: [
    (route {
      fromClass = "lsp-servers";
      intoClass = "nvim";
      path = [
        "lspmux"
        "servers"
      ];
      collectSubtree = true;
    })
  ];

  den.aspects.lspmux = { user, ... }: {
    includes = [
      den.aspects.packages.lspmux
      #        den.aspects.packages.kanata-ls
      lspServersToOverlays
      den.policies.lsp-servers-to-homeManager
      den.policies.lsp-servers-to-nvim
    ];

    # The "core" servers that have no dedicated language aspect of their own.
    # idris2_lsp / rust_analyzer live with their language aspects (dev/idris.nix,
    # dev/rust.nix) and are contributed to the same class from there.
    #
    # The class body takes `pkgs` as an arg (injected via each collector's
    # adaptArgs). For the `overlays` forward that builds `pkgs.lspmuxed`, `pkgs` is
    # bound to the overlay's `super`/`prev` -- the pinned fallback package resolves
    # against the project's own package set rather than a bare nixpkgs.
    lsp-servers =
      { pkgs, ... }:
      {
        clangd = {
          package = pkgs.clang-tools;
          exe = "clangd";
          zed = "clangd";
        };
        dhall_lsp_server = {
          package = pkgs.dhall-lsp-server;
          exe = "dhall-lsp-server";
        };
        docker_compose_language_service = {
          package = pkgs.docker-compose-language-service;
          exe = "docker-compose-langserver";
          args = [ "--stdio" ];
        };
        gopls = {
          package = pkgs.gopls;
          exe = "gopls";
          zed = "gopls";
        };
        hls = {
          package = pkgs.haskell-language-server;
          exe = "haskell-language-server-wrapper";
          args = [ "--lsp" ];
          zed = "hls";
          opencode = "haskell-language-server";
        };
        jdtls = {
          package = pkgs.jdt-language-server;
          exe = "jdtls";
          dynamicCmd = true;
          zed = "jdtls";
        };
        kotlin_language_server = {
          package = pkgs.kotlin-language-server;
          exe = "kotlin-language-server";
          zed = "kotlin-language-server";
          opencode = "kotlin-ls";
        };
        lua_ls = {
          package = pkgs.lua-language-server;
          exe = "lua-language-server";
          zed = "lua-language-server";
          opencode = "lua-ls";
        };
        metals = {
          package = pkgs.metals;
          exe = "metals";
          zed = "metals";
        };
        nil_ls = {
          package = pkgs.nil;
          exe = "nil";
          zed = "nix";
          opencode = "nixd";
        };
        ocamllsp = {
          package = pkgs.ocamlPackages.ocaml-lsp;
          exe = "ocamllsp";
          zed = "ocamllsp";
          opencode = "ocaml-lsp";
        };
        pyright = {
          package = pkgs.pyright;
          exe = "pyright-langserver";
          args = [ "--stdio" ];
          zed = "pyright";
          opencode = "pyright";
        };
        tailwindcss = {
          package = pkgs.tailwindcss-language-server;
          exe = "tailwindcss-language-server";
          dynamicCmd = true;
          zed = "tailwindcss-language-server";
        };
        ts_ls = {
          package = pkgs.typescript-language-server;
          exe = "typescript-language-server";
          dynamicCmd = true;
          zed = "typescript-language-server";
        };
        zls = {
          package = pkgs.zls;
          exe = "zls";
          zed = "zls";
        };
      };

    # The shim builder, as an overlay so it is reachable as `pkgs.wrapLspMux` from
    # every consumer: the `pkgs.lspmuxed` twins (via `buildLspmuxed` above), nvim, and
    # opencode all call it to wrap a server spec. The per-server `pkgs.lspmuxed`
    # entries themselves are contributed by the `lsp-servers` -> `overlays` forward.
    overlays = {
      lspmux-base = [
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
          #
          # `...` absorbs the rest of an `lspSubModule` (opencode, generic_name,
          # server_config, ...) so a full registry entry can be passed straight in.
          wrapLspMux =
            {
              package,
              exe ? baseNameOf (lib.getExe package),
              lspconfig ? exe,
              args ? [ ],
              dynamicCmd ? false,
              # Whether the lspmux `nvim` body should wire this into `lsp.servers`.
              # Set false for a server nvim drives another way (rust -> rustaceanvim)
              # while still wanting the multiplexed `pkgs.lspmuxed.<name>` twin.
              nvim ? true,
              # zed's own server name for this language, or null to not surface it in
              # zed. zed keys `lsp.<name>` by its own vocabulary (`nix`, not `nil_ls`),
              # so it cannot be derived from the lspconfig/exe/package keys.
              generic_name ? exe,
              zed ? generic_name,
              ...
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
                    nvim
                    zed
                    ;
                };
              };

              meta = (package.meta or { }) // {
                mainProgram = exe;
              };
            };
          # `pkgs.lspmuxed` is deliberately *not* an override of the top-level attrs:
          # clobbering pkgs.rust-analyzer would multiplex it inside unrelated build
          # inputs and mass-rebuild everything that depends on it.
        })
      ];
    };

    # Point nvim's language servers at the shims. This lives here rather than in the
    # nixvim aspect so that lspmux stays a single opt-in: include this aspect and the
    # editors get multiplexed, drop it and they go back to spawning servers directly.
    #
    # Driven off `config.lspmux.servers` -- the `lsp-servers` class routed into the
    # nvim module system (see `lsp-servers-to-nvim`) -- rather than the `pkgs.lspmuxed`
    # overlay, so nvim consumes the typed registry directly. Each shim is built with
    # `pkgs.wrapLspMux`. A registered server is enabled by default; `nvim = false`
    # (rust -> rustaceanvim, idris -> plugins.idris2) opts out. `server_config` (the
    # server's own settings, e.g. rust-analyzer's `check.command`) becomes `settings`.
    nvim =
      {
        config,
        pkgs,
        lib,
        ...
      }:
      {
        options.lspmux.servers = lib.mkOption {
          type = lib.types.attrsOf lspSubModule;
          default = { };
          description = "Language servers (the `lsp-servers` class) routed into the nvim module system.";
        };

        config.lsp.servers = lib.mapAttrs (
          lspconfig: spec:
          let
            shim = pkgs.wrapLspMux (spec // { inherit lspconfig; });
            # The vim.lsp.config block: `cmd` for statically-spawned servers plus the
            # server's own `settings` from `server_config`.
            cfg =
              (lib.optionalAttrs (!spec.dynamicCmd) { cmd = [ (lib.getExe shim) ] ++ spec.args; })
              // (lib.optionalAttrs (spec.server_config != { }) { settings = spec.server_config; });
          in
          {
            enable = lib.mkDefault true;
          }
          // (
            if spec.dynamicCmd then
              # lspconfig builds this server's cmd in a lua function at runtime, and we
              # cannot replace it without losing that logic (jdtls derives `-data
              # <workspace>`, ts_ls prefers a project-local node_modules server). So
              # shadow the binary name on $PATH instead: the function spawns it by bare
              # name, lands in the shim, and its computed args are forwarded verbatim.
              { package = shim; }
            else
              # Spawn the shim by path; it resolves the real server off $PATH at
              # runtime. `package = null` keeps nixvim from prefixing the pinned
              # server onto nvim's $PATH, where it would beat the project's own copy.
              { package = null; }
          )
          // lib.optionalAttrs (cfg != { }) { config = cfg; }
        ) (lib.filterAttrs (_lspconfig: spec: spec.nvim) config.lspmux.servers);
      };

    homeManager =
      {
        config,
        pkgs,
        lib,
        ...
      }:
      {
        options.lsp.servers = lib.mkOption {
          type = lib.types.attrsOf lspSubModule;
          default = { };
          description = ''
            Language servers multiplexed through lspmux, populated from the `lsp-servers`
            den class. The nvim side reads its own `lspmux.servers` twin; opencode reads
            this one below.
          '';
        };

        config = {
          home.packages = [ pkgs.lspmux ];

          # Feed the registry into opencode's LSP config, overriding opencode's own
          # built-in server ids (`rust`, `typescript`, `gopls`, ...) so their command
          # runs through the lspmux shim. Only servers whose `opencode` name is one of
          # those built-ins are surfaced -- overriding a built-in needs no `extensions`.
          # `server_config` (the server's own settings) becomes opencode's
          # `initialization` options. Lives here, gated on opencode being enabled, so
          # lspmux stays one opt-in.
          programs.opencode.settings.lsp = lib.mkIf config.programs.opencode.enable (
            lib.mapAttrs' (
              lspconfig: spec:
              lib.nameValuePair spec.opencode (
                {
                  command = [ (lib.getExe (pkgs.wrapLspMux (spec // { inherit lspconfig; }))) ] ++ spec.args;
                }
                // lib.optionalAttrs (spec.server_config != { }) { initialization = spec.server_config; }
              )
            ) (lib.filterAttrs (_lspconfig: spec: builtins.elem spec.opencode opencodeBuiltins) config.lsp.servers)
          );
        };
      };

    hmLinux =
      {
        config,
        pkgs,
        ...
      }:
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
      {
        config,
        pkgs,
        ...
      }:
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
