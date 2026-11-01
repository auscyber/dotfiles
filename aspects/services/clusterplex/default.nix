{
  den,
  lib,
  caps,
  ...
}:
# ClusterPlex (github.com/pabloromeo/clusterplex) on the native NixOS Plex:
# the PMS side is a patched `plexRaw` whose `Plex Transcoder` is ClusterPlex's
# node shim, the orchestrator runs beside it, and workers on other hosts take
# `transcode` and mount the media and transcode paths over `nfs`.
let
  cp = den.lib.clusterplex;
in
{
  den.lib.clusterplex = {
    ports = {
      orchestrator = 3500;
      relay = 32499;
      worker = 3501;
    };

    # Must be identical on PMS and every worker.
    transcodeDir = "/mnt/hdd/plex-transcode";
    mediaRoots = [ "/mnt/hdd/Media" ];

    # The host the PMS shim reports progress to; each worker's rewriter swaps
    # it for the relay address that worker resolved.
    pmsPlaceholder = "clusterplex-pms";

    packages =
      pkgs:
      let
        version = "0-unstable-2025-10-12";
        src = pkgs.fetchFromGitHub {
          owner = "pabloromeo";
          repo = "clusterplex";
          rev = "3958a77f533075b4069ef54a4689d27fac5c98e6";
          hash = "sha256-MPGwctLvGERvN9GD+QZoony/9Au/Ge1uGnVbkQegvb4=";
        };
        app =
          name: dir: npmDepsHash:
          pkgs.buildNpmPackage {
            pname = "clusterplex-${name}";
            inherit version npmDepsHash;
            src = "${src}/${dir}";
            dontNpmBuild = true;
            installPhase = ''
              runHook preInstall
              cp -r . $out
              runHook postInstall
            '';
          };
      in
      {
        inherit src;
        pms = app "pms" "pms/app" "sha256-w65l6inl0UoTJNqQXEbJVSFINYMCex+z4kZxbR59Q9I=";
        orchestrator =
          app "orchestrator" "orchestrator"
            "sha256-OfJ9lv4HlZTE9r2480D1Y/GhXo670U06a3z2QKSbY6k=";
        worker = app "worker" "worker/app" "sha256-sTcrARmryIZ3dMZNRlWTfhGGYp0OBJh/Gp6Aqzxlm4g=";
      };

    # Plex with `Plex Transcoder` swapped for the ClusterPlex shim. Built from
    # the same `plexRaw` a worker runs, so transcoder and codec versions match.
    plex =
      pkgs:
      let
        inherit (cp.packages pkgs) pms;
        env = {
          ORCHESTRATOR_URL = "http://127.0.0.1:${toString cp.ports.orchestrator}";
          PMS_IP = cp.pmsPlaceholder;
          LOCAL_RELAY_ENABLED = "1";
          LOCAL_RELAY_PORT = toString cp.ports.relay;
          TRANSCODER_LOCAL_NAME = "originalTranscoder";
          TRANSCODE_OPERATING_MODE = "both";
          TRANSCODE_EAE_LOCALLY = "true";
        };
      in
      pkgs.plex.override {
        plexRaw = pkgs.plexRaw.overrideAttrs (old: {
          nativeBuildInputs = old.nativeBuildInputs ++ [ pkgs.makeWrapper ];
          postInstall = (old.postInstall or "") + ''
            root="$out/lib/plexmediaserver"
            mv "$root/Plex Transcoder" "$root/originalTranscoder"
            makeWrapper ${lib.getExe pkgs.nodejs} "$root/Plex Transcoder" \
              --add-flags ${pms}/transcoder.js \
              --unset LD_LIBRARY_PATH \
              --set TRANSCODER_PATH "$root/" \
              ${lib.concatStringsSep " " (lib.mapAttrsToList (k: v: "--set ${k} ${lib.escapeShellArg v}") env)}
          '';
        });
      };

    nvenc =
      {
        pkgs,
        relayTo,
      }:
      pkgs.writers.writePython3Bin "plex-nvenc" { flakeIgnore = [ "E501" ]; } (
        lib.replaceStrings
          [
            "@transcoder@"
            "@driverLibs@"
            "@relayFrom@"
            "@relayTo@"
          ]
          [
            "${pkgs.plexRaw}/lib/plexmediaserver/Plex Transcoder"
            "/run/opengl-driver/lib"
            "${cp.pmsPlaceholder}:${toString cp.ports.relay}"
            relayTo
          ]
          (builtins.readFile ./nvenc_rewrite.py)
      );
  };

  den.capabilities.transcode = {
    meta.description = "A ClusterPlex orchestrator workers register with, plus the PMS relay their transcoders report progress to. A narrowing of tcp.";
    side = "endpoint";
    includes = [ caps.tcp ];
  };

  den.aspects.clusterplex-orchestrator = {
    reach = "household";

    gives =
      { self, ... }:
      let
        ports = {
          orchestrator = {
            port = cp.ports.orchestrator;
            meta.description = "ClusterPlex orchestrator (socket.io): the PMS shim on loopback, workers from the lan.";
          };
          relay = {
            port = cp.ports.relay;
            meta.description = "Local relay to Plex's transcode session API, dialled by remote transcoders.";
          };
        };
      in
      {
        tcp = _: {
          name = self.name;
          inherit ports;
        };
        transcode = _: {
          name = self.name;
          inherit ports;
        };
      };

    nixos =
      { pkgs, ... }:
      let
        inherit (cp.packages pkgs) orchestrator;
        location = {
          proxyPass = "http://127.0.0.1:32400";
          proxyWebsockets = true;
          extraConfig = ''
            proxy_request_buffering off;
          '';
        };
      in
      {
        systemd.services.clusterplex-orchestrator = {
          description = "ClusterPlex transcode orchestrator";
          wantedBy = [ "multi-user.target" ];
          after = [ "network.target" ];
          environment.LISTENING_PORT = toString cp.ports.orchestrator;
          serviceConfig = {
            ExecStart = "${lib.getExe pkgs.nodejs} ${orchestrator}/server.js";
            WorkingDirectory = orchestrator;
            DynamicUser = true;
            Restart = "on-failure";
          };
        };

        services.nginx.virtualHosts.clusterplex-relay = {
          listen = [
            {
              addr = "0.0.0.0";
              port = cp.ports.relay;
            }
          ];
          locations."/video/:/transcode/session/" = location;
          locations."/livetv/" = location;
        };
      };
  };

  # Separate from the worker: an aspect resolves one take per parent
  # capability, and `nfs` and `transcode` both narrow tcp.
  den.aspects.clusterplex-mounts = {
    includes = [ den.aspects.nfs-client ];

    takes.nfs.mounts = lib.genAttrs cp.mediaRoots (_: "ro") // {
      ${cp.transcodeDir} = "rw";
    };

    # Owns this host's copy of the nfs client key; tlshd loads it as root.
    gives =
      { self, ... }:
      {
        user = _: {
          name = "root";
          group = "root";
          carrier = self.name;
        };
      };
  };

  den.aspects.clusterplex-worker = {
    includes = [ den.aspects.clusterplex-mounts ];

    takes.transcode.name = "clusterplex-orchestrator";

    nixos =
      {
        config,
        pkgs,
        host,
        ...
      }:
      let
        d = den.lib.capability.dial "transcode" { name = "clusterplex-orchestrator"; } host;
        endpoint = n: "${d.address}:${toString d.ports.${n}.port}";
        inherit (cp.packages pkgs) src worker;
        nvenc = cp.nvenc {
          inherit pkgs;
          relayTo = endpoint "relay";
        };
        start = pkgs.writeShellApplication {
          name = "clusterplex-worker";
          runtimeInputs = with pkgs; [
            binutils
            gnugrep
            curl
            coreutils
          ];
          text = ''
            ver=$(strings "${pkgs.plexRaw}/lib/plexmediaserver/Plex Transcoder" | grep -Pom1 '[0-9a-f]{7}-[0-9a-f]{4,}$')
            dir="$STATE_DIRECTORY/codecs/$ver-linux-x86_64-standard"
            mkdir -p "$dir"
            while read -r codec; do
              [ -f "$dir/$codec.so" ] && continue
              curl -fsSL -o "$dir/$codec.so.part" "https://downloads.plex.tv/codecs/$ver/linux-x86_64-standard/$codec.so" \
                && mv "$dir/$codec.so.part" "$dir/$codec.so" \
                || echo "clusterplex-worker: no $codec for $ver" >&2
            done < ${src}/worker/app/codecs.txt
            export FFMPEG_EXTERNAL_LIBS="$dir/"
            exec ${lib.getExe pkgs.nodejs} ${worker}/worker.js
          '';
        };
      in
      lib.mkIf (d != null) {
        users.users.plex = {
          uid = config.ids.uids.plex;
          group = "plex";
          extraGroups = [ "media" ];
          isSystemUser = true;
        };
        users.groups.plex.gid = config.ids.gids.plex;
        users.groups.media.gid = 1000;

        systemd.services.clusterplex-worker = {
          description = "ClusterPlex transcode worker";
          wantedBy = [ "multi-user.target" ];
          wants = [ "network-online.target" ];
          after = [
            "network-online.target"
            "remote-fs.target"
          ];
          unitConfig.RequiresMountsFor = cp.mediaRoots ++ [ cp.transcodeDir ];
          environment = {
            ORCHESTRATOR_URL = "http://${endpoint "orchestrator"}";
            LISTENING_PORT = toString cp.ports.worker;
            TRANSCODER_PATH = "${nvenc}/bin/";
            TRANSCODER_NAME = "plex-nvenc";
            EAE_SUPPORT = "0";
            PLEX_ARCH = "amd64";
            PLEX_MEDIA_SERVER_INFO_MODEL = "x86_64";
            HOSTNAME = host.name;
          };
          serviceConfig = {
            ExecStart = lib.getExe start;
            User = "plex";
            Group = "plex";
            StateDirectory = "clusterplex-worker";
            WorkingDirectory = worker;
            Restart = "on-failure";
          };
        };
      };
  };

  perSystem =
    { pkgs, ... }:
    {
      checks.clusterplex-nvenc-rewrite =
        pkgs.runCommand "clusterplex-nvenc-rewrite-tests" { nativeBuildInputs = [ pkgs.python3 ]; }
          ''
            cp ${./nvenc_rewrite.py} nvenc_rewrite.py
            cp ${./test_nvenc_rewrite.py} test_nvenc_rewrite.py
            python3 -I test_nvenc_rewrite.py
            touch $out
          '';
    };
}
