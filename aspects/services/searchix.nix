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
# (agenix, disko, stylix, impermanence, ...). See ../docs/_searchix-sources.nix.
let
  # searchix's own web listener. It stays on loopback; nginx fronts it.
  port = 51313;

  # The option sources are built and published by CI
  # (.github/workflows/searchix-sources.yml): evaluating them here pulled every
  # other host's option tree into this host's eval, ~10GB of it.
  sourcesURL = "https://auscyber.github.io/dotfiles/searchix";

  sources = lib.imap0 (order: s: s // { inherit order; }) [
    {
      key = "dendritic";
      name = "Dendritic";
    }
    {
      key = "nixos";
      name = "NixOS";
    }
    {
      key = "home-manager";
      name = "Home Manager";
    }
    {
      key = "nixvim";
      name = "Nixvim";
    }
    {
      key = "darwin";
      name = "nix-darwin";
    }
  ];

  domain = "search.ivymect.in";
in
{
  den.aspects.searchix = {
    # `search.<zone>`, from the `domain` literal above rather than from a
    # `takes.proxy` -- so ../network/roaming-resolver.nix cannot derive it.
    alsoPublishes = [ "search" ];

    # Declared on the aspect, not the file: the input set follows which
    # hosts pull this aspect in, and so does its partition.
    inputs.searchix = {
      url = "git+https://git.sr.ht/~alanpearce/searchix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    includes = [ den.aspects.nginx ];

    # Public entry point.
    vhosts.${domain} = {
      useACMEHost = "ivymect.in";
      forceSSL = true;
      locations."/".proxyPass = "http://127.0.0.1:${toString port}";
    };

    nixos =
      let
        mkSource = src: {
          inherit (src) key name order;
          enable = true;
          fetcher = "download";
          importer = "options";
          url = "${sourcesURL}/${src.key}";
          # options.json is a flat { "option.name": {...} } object.
          jsonDepth = 1;
          timeout = "5m";
          repo = {
            type = "github";
            owner = "auscyber";
            repo = "dotfiles";
          };
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

            importer.sources = lib.listToAttrs (map (s: lib.nameValuePair s.key (mkSource s)) sources) // {
              # Upstream sources, all fetched over the network and rebuilt daily.
              # Package search wants a ~15min nix build of packages.json, so it
              # stays off until asked for; flip `enable` to get it.
              nixpkgs.enable = false;
              nur.enable = false;
            };
          };
        };

        systemd.services.searchix = {
          after = [ "network-online.target" ];
          wants = [ "network-online.target" ];
        };
      };
  };
}
