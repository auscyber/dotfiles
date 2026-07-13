{
  den,
  inputs,
  lib,
  ...
}:
# searchix (https://git.alin.ovh/searchix) — option search over *our* evaluated
# configurations rather than the upstream channels: the sources it indexes are
# generated from this flake's nixos/darwin/home-manager/nixvim option trees, so
# they include every option our aspects declare and every module we import
# (agenix, disko, stylix, impermanence, ...). See ../../docs/_searchix-sources.nix.
let
  # searchix's own web listener. It stays on loopback; nginx fronts it.
  port = 51313;

  # Static server for the generated option sources. searchix's `download`
  # fetcher is HTTP-only (it does url.JoinPath(URL, "options.json")), so the
  # store paths have to be served rather than read from disk.
  sourcePort = 51314;

  domain = "search.ivymect.in";
in
{
  flake-file.inputs.searchix = {
    url = "git+https://git.alin.ovh/searchix?ref=refs/tags/v0.4.4";
    inputs.nixpkgs.follows = "nixpkgs";
  };

  den.aspects.searchix = {
    includes = [ den.aspects.nginx ];

    # Public entry point. (The loopback source server lives in the `nixos`
    # module below instead of here, because its root is a derivation and so
    # needs `pkgs`, which the static `vhosts` class content doesn't get.)
    vhosts.${domain} = {
      useACMEHost = "ivymect.in";
      forceSSL = true;
      locations."/".proxyPass = "http://127.0.0.1:${toString port}";
    };

    nixos =
      { pkgs, ... }:
      let
        optionSources = import ../../docs/_searchix-sources.nix { inherit lib pkgs inputs; };

        mkSource =
          _: src:
          {
            inherit (src) key name order;
            enable = true;
            fetcher = "download";
            importer = "options";
            url = "http://127.0.0.1:${toString sourcePort}/${src.key}";
            # options.json is a flat { "option.name": {...} } object.
            jsonDepth = 1;
            timeout = "5m";
            repo = optionSources.repo;
          };
      in
      {
        imports = [ inputs.searchix.nixosModules.web ];

        services.searchix = {
          enable = true;

          settings = {
            web = {
              inherit port;
              listenAddress = "127.0.0.1";
              baseURL = "https://${domain}";
            };

            importer.sources = lib.mapAttrs mkSource optionSources.meta // {
              # Upstream sources, all fetched over the network and rebuilt daily.
              # Package search wants a ~15min nix build of packages.json, so it
              # stays off until asked for; flip `enable` to get it.
              nixpkgs.enable = false;
              nur.enable = false;
            };
          };
        };

        # The importer runs on startup, and it fetches its sources over HTTP from
        # the nginx below.
        systemd.services.searchix = {
          after = [ "nginx.service" ];
          wants = [ "nginx.service" ];
        };

        services.nginx.virtualHosts.searchix-sources = {
          # Loopback-only; no server_name matching, this port serves nothing else.
          serverName = null;
          listen = [
            {
              addr = "127.0.0.1";
              port = sourcePort;
            }
          ];
          root = optionSources.tree;
          locations."/".extraConfig = ''
            # Store files are all mtime 1970, so nginx would answer searchix's
            # conditional requests with a permanent 304 and it would never see a
            # new revision. Serve unconditionally and let the `revision` file
            # decide whether a re-import is needed.
            if_modified_since off;
            etag off;
          '';
        };
      };
  };
}
