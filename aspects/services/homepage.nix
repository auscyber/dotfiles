{
  lib,
  den,
  ...
}:
# gethomepage, fed by a `homepage` den class so a service aspect declares its
# own dashboard tile next to the service it belongs to.
#
# `services.homepage-dashboard.services` wants a list of single-key attrsets
# nested two deep -- [ { Group = [ { Name = { href; ... }; } ]; } ] -- which no
# two aspects can write into together, since list definitions only concatenate
# and group membership would be fixed by whichever aspect emitted the group
# first. The class collects a flat `homepage.entries.<name>` attrset instead,
# and the `nixos` body below is the one place that folds it into that shape.
let
  port = 8082;

  # `vhosts` in ./nginx.nix is the model.
  homepageEntries =
    {
      class,
      aspect-chain,
    }:
    den.batteries.forward {
      each = lib.singleton true; # single forward; item ignored
      fromClass = _: "homepage";
      intoClass = _: "nixos";
      intoPath = _: [
        "homepage"
        "entries"
      ];
      fromAspect = _: lib.head aspect-chain;
      adaptArgs = lib.id;
    };
in
{
  den.aspects.homepage = {
    includes = [
      homepageEntries
      den.aspects.nginx
      den.aspects.gateway
    ];

    # Through the gateway rather than a plain vhost, so the dashboard is behind
    # kanidm like everything it links to. It is a map of the whole estate --
    # leaving it open would hand an unauthenticated visitor the index.
    gated.homepage.upstream = "http://127.0.0.1:${toString port}";

    nixos =
      { config, ... }:
      let
        inherit (lib) mkOption types;

        # The entry submodule shadows `config` with its own; this keeps a handle
        # on the host's, for the `enable` default below.
        outer = config;

        # `name`/`group`/`enable` are ours, everything left is a homepage field.
        # A null or an untouched `widget` would otherwise reach services.yaml as
        # `null` / `{}`, and homepage treats an empty widget as a broken one.
        toTile = entry: {
          ${entry.name} = lib.filterAttrs (k: v: v != null && !(k == "widget" && v == { })) (
            builtins.removeAttrs entry [
              "_module"
              "name"
              "group"
              "enable"
            ]
          );
        };

        live = lib.filter (e: e.enable) (lib.attrValues config.homepage.entries);
        byName = a: b: a.name < b.name;
      in
      {
        options.homepage.entries = mkOption {
          default = { };
          description = "Dashboard tiles, collected from every aspect's `homepage` class.";
          type = types.attrsOf (
            types.submodule (
              { name, ... }: {
                # Whatever else homepage accepts -- ping, siteMonitor,
                # statusStyle, container -- passes through untouched.
                freeformType = types.attrsOf types.raw;

                options = {
                  enable = mkOption {
                    type = types.bool;
                    default = outer.services.${name}.enable or true;
                    defaultText = "services.‹name›.enable, or true when there is no such service";
                    description = ''
                      A tile for a service that is switched off is a dead link,
                      so an entry keyed after a NixOS service follows that
                      service by default. Read `.enable` and nothing else here:
                      an option default that forces more of the host config
                      recurses (see lib/age-scoped.nix).
                    '';
                  };
                  name = mkOption {
                    type = types.str;
                    default = name;
                    description = "Tile label.";
                  };
                  group = mkOption {
                    type = types.str;
                    description = "Dashboard section the tile is listed under.";
                  };
                  href = mkOption {
                    type = types.str;
                    description = "Where the tile links to.";
                  };
                  description = mkOption {
                    type = types.nullOr types.str;
                    default = null;
                  };
                  icon = mkOption {
                    type = types.nullOr types.str;
                    default = null;
                    example = "sonarr";
                    description = "A dashboard-icons name, an `mdi-`/`si-` name, or a URL.";
                  };
                  widget = mkOption {
                    type = types.attrsOf types.raw;
                    default = { };
                    example = {
                      type = "sonarr";
                      url = "http://127.0.0.1:8989/sonarr";
                    };
                    description = ''
                      Passed to homepage verbatim, so the API-key field is
                      whatever the widget type calls it -- usually `key`.
                    '';
                  };
                };
              }
            )
          );
        };

        config.services.homepage-dashboard = {
          enable = true;
          listenPort = port;
          # Widget API keys come in as `{{HOMEPAGE_VAR_<SERVICE>_KEY}}`; systemd
          # reads this as root, so the DynamicUser unit never needs the file.
          environmentFile = config.age.templates."gateway/account-homepage.env".path;
          # A comma-separated string, not a list, and the default covers only
          # localhost -- anything else gets a bare 400 out of next.js.
          # A missing hostname here is a bare 400 from next.js, so it has to
          # track whatever the gateway publishes it as.
          allowedHosts = "homepage.${config.gateway.domain},localhost:${toString port},127.0.0.1:${toString port}";
          # Groups come out in `attrNames` order, so both levels are alphabetical.
          services = lib.mapAttrsToList (g: es: { ${g} = map toTile (lib.sort byName es); }) (
            lib.groupBy (e: e.group) live
          );
        };
      };
  };
}
