{
  lib,
  den,
  caps,
  capability,
  checkLib,
  ...
}:
# gethomepage, and the dashboard tile AS A CAPABILITY the described service
# gives.
#
# ── WHICH DIRECTION, AND WHY THIS ONE ──
#
# "this homepage stuff also seems more like a capability than a plain forward
# like we have at the moment so we can pull it properly with provenance like
# `gives.homepage-widget.plex`".
#
# So the SERVICE gives and homepage reads, which is the same shape as
# `den.capabilities.service` (../base/service.nix): many givers, one reader, the
# definition living in the file that models the abstraction. The inverse --
# homepage gives a dashboard and services take it -- was considered and is
# wrong twice over:
#
#   * A widget is the SERVICE's fact. Which gethomepage widget type draws it,
#     which icon names it, which section it belongs in: homepage knows none of
#     that about plex, and a `taker.options` block holding all of it would be
#     the current forward class under a new name.
#   * `capability.contributions` folds a per-taker contribution into ONE
#     value per taker, so a take could not carry a tile that is not named after
#     the taking aspect -- and the tile's whole point is provenance.
#
# The capability is named after gethomepage rather than something generic
# (`dashboard`), deliberately: `widget.type` is gethomepage's own catalogue of
# widget names, so a capability called `dashboard` would claim a generality it
# does not have.
#
# ── WHAT WAS HERE BEFORE ──
#
# A den class, `homepage`, forwarded per-aspect content into
# `homepage.entries.<name>`. It worked and it had no provenance: the entries
# were untyped (a misspelled field reached services.yaml as a key homepage
# ignores), nothing recorded which aspect furnished one, and a tile could be
# written by any aspect for any service. `gives.homepage-widget` is typed by the
# capability's own `options` below, so a misspelling fails AT THE GIVER, and
# `carrier` on the resolved link says who furnished it.
#
# `services.homepage-dashboard.services` wants a list of single-key attrsets
# nested two deep -- [ { Group = [ { Name = { href; ... }; } ]; } ] -- which no
# two aspects can write into together, since list definitions only concatenate
# and group membership would be fixed by whichever aspect emitted the group
# first. `homepage.entries` survives as the flat attrset the `nixos` body folds
# into that shape; what changed is that the capability DEFINES it rather than a
# forward class.
#
# ── ACCESS: ONE PAGE PER MEMBER, PICKED BY THE GATE ──
#
# Homepage renders ONE services.yaml per INSTANCE and has no user model of its
# own: 2.0 added an authorization GATE (`HOMEPAGE_AUTH_ENABLED` with a password
# or OIDC) and nothing more -- "Homepage grants access to any identity that the
# configured OIDC provider authorizes for this client". So filtering per viewer
# inside one instance is impossible, and the only way to show Ivy her services
# and Jonesy his is an instance each. That is what this file builds:
#
#   * `takes.gated.forwardUser` makes ../services/gateway.nix forward the
#     authenticated person as `Remote-User`. Its value is the kanidm ACCOUNT
#     name -- ivy's is `auscyber` -- so the routing is keyed on
#     `person.kanidm.name`, exactly as ./access.nix's groups are. Keying on the
#     member name would send her, alone among four, to the fallback page.
#   * `port` is still what the gate forwards to, but it is now an nginx server
#     block that `map`s that header onto one of the pages below.
#   * one nspawn CONTAINER per member (`homepageLib.containersFor`), each its
#     own entity on the nspawn bridge, showing exactly the tiles that member
#     has a grant for. The router dials each one, so there is no port band.
#   * the nixpkgs instance stays as the FALLBACK page, for an authenticated
#     identity that is no member of this household. It keeps the all-viewers
#     filter, which leaks nothing to a viewer we know nothing about.
#
# Adding a member to ../households/ivymectin.nix is therefore the only edit:
# the container, its address and the map entry all follow from the member
# list.
let
  port = 8082; # the nginx router -- what the gate forwards to
  # `+ 2` AND NOT `+ 1`: 8083 is tandoor's (./tandoor-recipes.nix).
  sharedPort = port + 2; # the fallback page, which is the nixpkgs instance
  # Inside each member's container, which has a namespace of its own.
  pagePort = 8082;

  # ── THE REAL HOST ENTITY, NOT THE PROJECTED ONE ──
  #
  # `providersFor` asks `host.hasAspect`, and a class body's `host` carries
  # den's PROJECTED version, which answers "delivered into this scope" rather
  # than "does that host have it" -- the trap ../base/firewall.nix and
  # ./shares.nix were both bitten by, and which ../base/capability.nix's own
  # `bindAddressFor` resolves on this side for the same reason.
  hostsOf = lib.concatMapAttrs (_: hs: hs) den.hosts;
  entityFor = host: hostsOf.${host.name or ""} or host;

  # ── THE TILES ON A HOST, RESOLVED THROUGH THE CAPABILITY ──
  #
  # Hoisted to file level for the reason ./kanidm.nix gives: a fold over
  # `den.aspects` and a host's own inclusion set needs no host fixpoint, while
  # the same expression inside a class body can only be forced by a full host
  # evaluation.
  #
  # `peer = den.aspects.homepage` -- the reader is this dashboard, so a giver
  # that wants to furnish a different tile per consumer can. None does yet.
  tilesFor =
    host:
    let
      h = entityFor host;
    in
    lib.mapAttrs (
      _: aspect:
      capability.linkFor aspect "homepage-widget" {
        host = h;
        peer = den.aspects.homepage;
      }
    ) (capability.providersFor h "homepage-widget");

  # ── THE API KEY STAYS AN EDGE ──
  #
  # Homepage holds ONE gateway service-account key and presents it to every
  # service it draws, which is `takes.credential` below and nothing new. The
  # placeholder is composed from that take rather than written out again, so
  # the prefix is stated once: ./gateway.nix renders `${envPrefix}KEY` into the
  # account's env file, and this is the same name read back.
  #
  # A RAW TAKE, so `or` is the correct operator -- `aspect.takes.<cap>` carries
  # only the keys the taker actually set, and a `takerOption`'s default does not
  # reach it (../../CLAUDE.md).
  keyPlaceholder =
    let
      prefix = den.aspects.homepage.takes.credential.envPrefix or null;
    in
    if prefix == null then null else "{{${prefix}KEY}}";

  # WHETHER A SERVICE ADMITS HOMEPAGE'S KEY, derived rather than restated. The
  # callee is the end that says the pair exists (./gateway.nix's `principals`
  # is the same reading), so a service naming `homepage` in `api.clients` is
  # exactly a service whose widget may present that key -- and one that does
  # not gets a widget with no credential in it rather than a 401 on every
  # refresh.
  admitsHomepage = aspect: lib.elem "homepage" (aspect.takes.gated.clients or [ ]);

  # ── WHO MAY REACH A SERVICE, the same walk ./access.nix makes ──
  #
  # Both arms, because a grant names either a service or a service GROUP: the
  # *arr apps declare no `access` of their own and are reachable entirely
  # through the household's `arr` group, so reading only the service's own
  # character would call all six ungated.
  #
  # `null` means UNGATED rather than "nobody": a service the household does not
  # declare, or declares with no access character on either arm, emits no
  # `app-*` group at all (./access.nix skips it), so nothing narrows who
  # reaches it and a tile for it may be shown to anyone who got through the
  # gate. plex, tautulli, seerr and navidrome are all this case.
  audienceFor =
    host: service:
    let
      h = entityFor host;
      household = h.household or null;
      svc = if household == null then null else household.services.${service} or null;

      # The group's levels, which is where a tagged service's grants live.
      # `serviceGroups.<g>.access` is `nullOr raw` with a null default, so this
      # is a test and not a fallback.
      groupAccess =
        if svc == null || svc.group == null then
          null
        else
          (household.serviceGroups.${svc.group} or { access = null; }).access;

      direct = if svc != null && svc.access != null then lib.attrNames svc.users else [ ];

      viaGroup =
        if groupAccess == null then
          [ ]
        else
          lib.attrNames (lib.filterAttrs (_: m: m.services ? ${svc.group}) household.members);
    in
    if svc == null || (svc.access == null && groupAccess == null) then
      null
    else
      lib.unique (direct ++ viaGroup);

  # Everyone the household has. The dashboard's own audience, because homepage
  # is not a declared household service and so nothing narrows who may open it
  # beyond the gate itself.
  membersOf =
    host:
    let
      h = entityFor host;
      household = h.household or null;
    in
    if household == null then [ ] else lib.attrNames household.members;

  containerOf = m: "homepage-${m}";

  # ── A CONTAINER PER MEMBER, FOR A HOST TO DECLARE ──
  # `den.hosts.<sys>.<h>.containers = homepageLib.containersFor household;`
  containersFor =
    household:
    lib.mapAttrs' (
      m: _:
      lib.nameValuePair (containerOf m) {
        runtime = "nspawn";
        aspect = den.aspects.homepage-page;
      }
    ) household.members;

  # EVERYTHING ONE PAGE NEEDS, in one place: its container, where the router
  # dials it, the header value that selects it, the title it shows. A member
  # whose container the host does not declare has no page and gets the
  # fallback.
  pagesFor =
    host:
    let
      h = entityFor host;
      household = h.household or null;
      members = if household == null then { } else household.members;
    in
    lib.mapAttrs (
      m: member:
      let
        c = containerOf m;
        # Every page is this one aspect; which one is the entity it runs on.
        d = capability.dial "tcp" {
          name = "homepage-page";
          where = cand: (cand.host.name or c) == c;
        } h;
      in
      {
        member = m;
        container = c;
        unit = "container@${c}";
        address = if d == null then null else d.address;
        port = if d == null then null else d.port;
        account = member.person.kanidm.name;
        title = member.person.displayName;
      }
    ) (lib.filterAttrs (m: _: (h.containers or { }) ? ${containerOf m}) members);

  # The tile's LABEL, which is also the name the gateway publishes it under.
  # `name` comes from `carried` with a null default, so this is a test and not a
  # fallback (../../CLAUDE.md).
  labelOf = aspectName: tile: lib.defaultTo aspectName tile.name;

  audienceOf =
    host: aspectName: tile:
    audienceFor host (lib.defaultTo (labelOf aspectName tile) tile.service);

  # ── THE FILTER, PARAMETERISED BY WHO IS LOOKING ──
  #
  # `subtractLists` rather than a subset helper: what is left is exactly the
  # viewers who would see a tile for something they have no grant on. A
  # dedicated page has ONE viewer, so this reads "that member has a grant"; the
  # fallback page's viewers are the whole household, so there it is the sound
  # filter it has always been.
  visibleTo = viewers: audience: audience == null || lib.subtractLists audience viewers == [ ];

  # Which tiles a set of viewers may see. Entities and givers only, so the
  # per-member filter answers without evaluating a host (../../CLAUDE.md):
  #
  #   nix eval --impure --json .#debug.config --apply 'cfg:
  #     cfg._module.args.homepageLib.tilesVisibleTo
  #       cfg.den.hosts.x86_64-linux.secondpc [ "ivy" ]'
  tilesVisibleTo =
    host: viewers:
    lib.attrNames (lib.filterAttrs (n: t: visibleTo viewers (audienceOf host n t)) (tilesFor host));

  # ── A PAGE A MEMBER CANNOT REACH IS A PAGE NOBODY SEES ──
  #
  # A member with no container, a container the router cannot dial, and two
  # members sharing one `Remote-User` -- the map's second entry would never
  # match, so one of them silently gets the other's page.
  pageFindings =
    host:
    let
      h = entityFor host;
      where = host.name or "?";
      pages = pagesFor host;
    in
    map (
      m:
      "homepage on ${where}: ${m} has no container (`${containerOf m}`), so they get the fallback page."
    ) (lib.subtractLists (lib.attrNames pages) (membersOf h))
    ++ lib.concatMap (
      p:
      lib.optional (p.address == null)
        "homepage on ${where}: nothing on ${where} can dial `${p.container}`, so the router has nowhere to send ${p.member}."
    ) (lib.attrValues pages)
    ++ lib.concatLists (
      lib.mapAttrsToList (
        v: ps:
        lib.optional (lib.length ps > 1)
          "homepage on ${where}: kanidm account `${v}` is shared by ${
            lib.concatMapStringsSep ", " (p: p.member) ps
          }, so only one of them gets a page of their own."
      ) (lib.groupBy (p: p.account) (lib.attrValues pages))
    );
in
{
  # The lib scope, not a path import (../../CLAUDE.md). It is also the fast
  # loop: `tilesVisibleTo` answers the per-member filter off entities alone.
  _module.args.homepageLib = {
    inherit
      port
      sharedPort
      containersFor
      pagesFor
      visibleTo
      tilesVisibleTo
      ;
  };

  den.checks.homepage-pages = {
    description = "Every member of a household served by homepage has a page of their own -- a container the router can dial, one kanidm account name each.";
    findings = lib.concatMap (
      h: checkLib.probe (h.name or "?") "homepage pages on ${h.name or "?"}" (pageFindings h)
    ) (lib.filter (h: h.hasAspect den.aspects.homepage) checkLib.hostEntities);
    fix = "Declare `containers = homepageLib.containersFor household;` on the host; a repeated account name is two people sharing `den.persons.<n>.kanidm.name`.";
  };

  # ── A DASHBOARD TILE, AS A CAPABILITY ──
  #
  # Declared HERE, in the aspect that READS it, which is the one case where the
  # owner is not the giver: there are many givers and one reader, so the reader
  # is what the capability is about. `den.capabilities.service`
  # (../base/service.nix) is the same arrangement and the precedent.
  #
  # NOT IN ../base/capability-types.nix, which keeps only base vocabulary.
  den.capabilities.homepage-widget = {
    meta.description = "A dashboard tile, declared by the service it describes.";
    includes = [ caps.carried ];

    options = {
      group = lib.mkOption {
        type = lib.types.str;
        example = "Media";
        description = "Dashboard section the tile is listed under. Homepage orders sections alphabetically.";
      };

      # ── WHAT IS NOT RESTATED HERE ──
      #
      # `href` defaults to null and nearly every giver leaves it there: the
      # gateway already derives `gateway.services.<n>.url` from the vhost and
      # the subpath it publishes a service at, and the `nixos` body below reads
      # that. A tile stating its own URL would be the published address written
      # a second time, with nothing keeping the two equal.
      #
      # Stated for a service published OUTSIDE the gateway -- plex sits on
      # `media.pierlot.com.au` as a plain vhost -- which is the case this
      # option exists for and the only one in the tree.
      href = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = "Where the tile links to. Null means the gateway's published url for this service.";
      };

      icon = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        example = "sonarr.svg";
        description = "A dashboard-icons name, an `mdi-`/`si-` name, or a URL.";
      };

      description = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = "Subtitle under the tile's name.";
      };

      # ── WHICH HOUSEHOLD SERVICE THIS TILE IS ABOUT ──
      #
      # The access question is asked of a SERVICE and the tile is keyed by the
      # ASPECT, and the two names differ wherever the household names a service
      # after what it is rather than after the program implementing it
      # (`home-assistant` is given by `den.aspects.homeassistant`). Null means
      # they are the same, which is the common case.
      service = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        example = "home-assistant";
        description = "The household service whose grants gate this tile. Null means the tile's own name.";
      };

      # ── HOMEPAGE'S OWN WIDGET BLOCK, VERBATIM BUT FOR TWO FIELDS ──
      #
      # `url` and the API-key field are filled in below rather than here: the
      # url is the tile's href, which the gateway derives, and the key is
      # homepage's own credential, which only homepage knows the prefix of. A
      # giver that sets `url` itself is believed -- plex's widget wants the
      # local 32400 rather than the published vhost.
      widget = lib.mkOption {
        type = lib.types.attrsOf lib.types.raw;
        default = { };
        example = {
          type = "sonarr";
        };
        description = "Passed to homepage verbatim. Empty means a link with no widget.";
      };

      # Which field of that block takes homepage's key. `key` for every widget
      # type in use here; null for a type that takes none, so a widget can
      # refuse the credential even though the service admits homepage as a
      # client.
      keyField = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = "key";
        description = "The widget field homepage's own API key is written into, or null for a widget that takes none.";
      };
    };

    # NO `taker` OPTIONS, AND SO NO `takes.homepage-widget` ANYWHERE. There is
    # one reader and it has nothing to state per giver, so the relation is read
    # with `providersFor` + `linkFor` exactly as ../../lib/service-type.nix
    # reads `service`. A take would be a declaration with no content.
  };

  # ── WHAT RUNS INSIDE A MEMBER'S CONTAINER ──
  # The dashboard on its own namespace's port, which the router dials over the
  # nspawn bridge. What it shows is rendered by the host (`pageFor` below).
  den.aspects.homepage-page = {
    gives = _: {
      tcp = _: {
        ports.default = {
          port = pagePort;
          meta.description = "One member's dashboard, inside their own container; the homepage router dials it over the nspawn bridge.";
        };
      };
    };
    nixos = {
      services.homepage-dashboard = {
        enable = true;
        listenPort = pagePort;
      };
      networking.firewall.interfaces.eth0.allowedTCPPorts = [ pagePort ];
    };
  };

  den.aspects.homepage = {
    includes = [
      den.aspects.nginx
      den.aspects.gateway
    ];

    # Through the gateway rather than a plain vhost, so the dashboard is behind
    # kanidm like everything it links to. It is a map of the whole estate --
    # leaving it open would hand an unauthenticated visitor the index.
    # ── WHAT IT LISTENS ON, so the proxy can ASK rather than assume ──
    #
    # Given rather than passed to the gate as a number: ../../base/capability.nix
    # composes this endpoint with whichever carrier the two ends share -- the
    # loopback for nginx on this box -- so ../proxy-nginx.nix resolves the
    # upstream rather than hardcoding one, and the same endpoint is what makes
    # this service's public https address derivable.
    #
    # NOT EXPOSED: loopback only, because the gate is the way in. The firewall
    # is derived from this (../../base/firewall.nix), so saying nothing here
    # leaves the port shut on every wire.
    #
    # EVERY SOCKET, not just the router's: the pages are sockets on this box
    # too, and declaring them is what lets `socket-exclusions` catch a page
    # colliding with another service. `default` and not `http` for the router,
    # because ../proxy-nginx.nix DIALS this endpoint and `dial` answers with
    # "the" port only for `default` or a single-port giver.
    gives =
      {
        self,
        host ? { },
        ...
      }:
      {
        tcp = _: {
          name = self.name;
          ports = {
            default = {
              inherit port;
              meta.description = "The nginx router the gate forwards to, which maps `Remote-User` onto one of the pages below.";
            };
            shared = {
              port = sharedPort;
              meta.description = "The fallback page's own dashboard instance -- what the router answers with for a visitor it cannot attribute to a member.";
            };
          };
        };
      };

    # `takes.gated` carries no port any more -- see `gives` above.
    #
    # `forwardUser` is the whole routing input: the gate sets `Remote-User` from
    # oauth2-proxy's `preferred_username`, and the nginx `map` below turns that
    # into a page. Without it every request looks the same and there is nothing
    # to route on.
    takes.gated.forwardUser = true;

    # ── THE OTHER DIRECTION: HOMEPAGE IS A CALLER ──
    #
    # Every widget drawn below reads `{{HOMEPAGE_VAR_KEY}}`, so homepage holds
    # one gateway key and presents it to each service it draws. The ACCOUNT is
    # not declared -- ../services/gateway.nix derives it from the services that
    # name `homepage` in `api.clients` -- and what is left is how this end
    # CONSUMES it, which only this end knows: the prefix the env file is
    # rendered under, and the unit that has to be restarted because a running
    # process keeps the environment it started with.
    #
    # `keyPlaceholder` above is composed from this, so the prefix is stated
    # here and nowhere else.
    takes.credential = {
      description = "Dashboard widgets";
      envPrefix = "HOMEPAGE_VAR_";
      restartUnits = [ "homepage-dashboard.service" ];
    };

    nixos =
      {
        config,
        host,
        pkgs,
        ...
      }:
      let
        inherit (lib) mkOption types;

        # The entry submodule shadows `config` with its own; this keeps a handle
        # on the host's, for the `enable` default below.
        outer = config;

        tiles = tilesFor host;
        pages = pagesFor host;
        viewers = membersOf host;

        hrefFor =
          aspectName: tile:
          if tile.href != null then
            tile.href
          else
            # A GENUINELY MISSING ATTRIBUTE, so `or` is right: a service with no
            # gateway entry has no `gateway.services.<n>` at all.
            config.gateway.services.${labelOf aspectName tile}.url or null;

        # `url` and the key, folded into whatever the giver furnished. The
        # giver's own value wins for both, since `widget` is freeform and a key
        # it never set is genuinely absent.
        widgetFor =
          aspectName: aspect: tile:
          let
            w = tile.widget;
            href = hrefFor aspectName tile;
            keyed = tile.keyField != null && keyPlaceholder != null && admitsHomepage aspect;
          in
          if w == { } then
            { }
          else
            lib.optionalAttrs (href != null) { url = href; }
            // lib.optionalAttrs (keyed) { ${tile.keyField} = keyPlaceholder; }
            // w;

        # `name`/`group`/`enable`/`audience` are ours, everything left is a
        # homepage field. A null or an untouched `widget` would otherwise reach
        # services.yaml as `null` / `{}`, and homepage treats an empty widget as
        # a broken one.
        toTile = entry: {
          ${entry.name} = lib.filterAttrs (k: v: v != null && !(k == "widget" && v == { })) (
            builtins.removeAttrs entry [
              "_module"
              "name"
              "group"
              "enable"
              "audience"
            ]
          );
        };

        byName = a: b: a.name < b.name;

        # ── ONE services.yaml PER PAGE ──
        # The entries are defined once with their audience on them; a page is
        # the subset its own viewers may see. Groups come out in `attrNames`
        # order, so both levels are alphabetical.
        servicesFor =
          vs:
          let
            live = lib.filter (e: e.enable && (!config.homepage.gateEntries || visibleTo vs e.audience)) (
              lib.attrValues config.homepage.entries
            );
          in
          lib.mapAttrsToList (g: es: { ${g} = map toTile (lib.sort byName es); }) (
            lib.groupBy (e: e.group) live
          );

        # A comma-separated string, not a list. A hostname missing from it is a
        # bare 400 out of next.js, so it tracks whatever the gateway publishes
        # this as -- and each page is reached on its own port.
        allowedHostsFor =
          p: "homepage.${config.gateway.domain},localhost:${toString p},127.0.0.1:${toString p}";

        # ── A PER-MEMBER INSTANCE, OFF THE NIXPKGS ONE ──
        sharedCfg = config.services.homepage-dashboard;
        envFile = config.age.templates."gateway/account-homepage.env".path;

        # A MEMBER'S PAGE, rendered here and handed to their container: the
        # tiles need this host's gateway urls, and the widget keys are this
        # host's template, bound in read-only at the same path.
        pageFor = pg: {
          bindMounts.${toString envFile} = {
            hostPath = toString envFile;
            isReadOnly = true;
          };
          config.services.homepage-dashboard = {
            environmentFile = envFile;
            allowedHosts = "homepage.${config.gateway.domain},${pg.address}:${toString pg.port}";
            services = servicesFor [ pg.member ];
            settings = sharedCfg.settings // {
              title = pg.title;
            };
            inherit (sharedCfg)
              widgets
              bookmarks
              docker
              kubernetes
              proxmox
              customCSS
              customJS
              ;
          };
        };

        # ── WHICH PAGE, FROM THE PERSON THE GATE FORWARDED ──
        # `$http_remote_user` is the `Remote-User` header ../services/gateway.nix
        # sets from `preferred_username`; it is empty on anything that did not
        # authenticate, which is the `default` arm. Keys are QUOTED so an account
        # name cannot be read as one of `map`'s own words.
        routerMap = lib.concatStringsSep "\n" (
          [
            "map $http_remote_user $homepage_upstream {"
            "    default 127.0.0.1:${toString sharedPort};"
          ]
          ++ map (pg: "    \"${pg.account}\" ${pg.address}:${toString pg.port};") (
            lib.filter (pg: pg.address != null) (lib.attrValues pages)
          )
          ++ [ "}" ]
        );
      in
      {
        options.homepage.gateEntries = mkOption {
          type = types.bool;
          default = true;
          description = ''
            Drop a tile from a page unless every viewer of THAT page can also
            reach the service it describes.

            A member's dedicated page has one viewer, so this reads "that
            member has a grant". The fallback page's viewers are the whole
            household, so there it is the sound filter: a tile that survives it
            is one nobody who lands on that page lacks a grant for. Off renders
            every tile on every page.
          '';
        };

        options.homepage.entries = mkOption {
          default = { };
          description = ''
            Dashboard tiles, each carrying the audience it is gated to. Defined
            from every aspect's `gives.homepage-widget` on this host and
            UNFILTERED: every page renders the subset its own viewers may see,
            so one definition serves the fallback page and each member's.

            Still an option rather than a `let`, because a host may add a tile
            for something that is not an aspect.
          '';
          type = types.attrsOf (
            types.submodule (
              { name, ... }:
              {
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
                      url = "https://arr.ivymect.in/tv";
                    };
                    description = ''
                      Passed to homepage verbatim, so the API-key field is
                      whatever the widget type calls it -- usually `key`.
                    '';
                  };
                  audience = mkOption {
                    type = types.nullOr (types.listOf types.str);
                    default = null;
                    description = ''
                      Household members granted the service this tile
                      describes. NULL IS UNGATED -- a service the household
                      does not declare narrows nobody -- so it shows on every
                      page, which is also the honest answer for a tile a host
                      added by hand.
                    '';
                  };
                };
              }
            )
          );
        };

        # ── THE ENTRIES, FROM THE CAPABILITY ──
        #
        # Keyed by the GIVING ASPECT and not by the tile's label: two aspects
        # cannot then collide on one key, and `enable` above follows
        # `services.<aspect>.enable`, which is the unit the tile links to for
        # every service in this tree.
        config.homepage.entries = lib.mapAttrs (aspectName: tile: {
          name = labelOf aspectName tile;
          inherit (tile) group icon description;
          href = hrefFor aspectName tile;
          widget = widgetFor aspectName den.aspects.${aspectName} tile;
          audience = audienceOf host aspectName tile;
        }) tiles;

        # THE FALLBACK PAGE, and the one instance nixpkgs' module renders.
        config.services.homepage-dashboard = {
          enable = true;
          listenPort = sharedPort;
          # Widget API keys come in as `{{HOMEPAGE_VAR_<SERVICE>_KEY}}`; systemd
          # reads this as root, so the DynamicUser unit never needs the file.
          environmentFile = config.age.templates."gateway/account-homepage.env".path;
          allowedHosts = allowedHostsFor sharedPort;
          services = servicesFor viewers;
        };

        config.containers = lib.mapAttrs' (_: pg: lib.nameValuePair pg.container (pageFor pg)) (
          lib.filterAttrs (_: pg: pg.address != null) pages
        );

        # Every page holds the widget key in memory, so every page restarts when
        # it is rotated. `takes.credential.restartUnits` names the shared one;
        # the members are only knowable with a host in hand, and this list
        # merges with what the gateway derives.
        config.gateway.serviceAccounts.homepage.restartUnits = map (pg: "${pg.unit}.service") (
          lib.attrValues pages
        );

        # `commonHttpConfig` and not the append variant: the server block below
        # reads `$homepage_upstream`, and nginx parses in order.
        config.services.nginx.commonHttpConfig = routerMap + "\n";

        # THE ROUTER. `proxy_pass` to a variable holding a literal `IP:port`
        # needs no `resolver` (a hostname would), and with no URI of its own it
        # passes the request URI through as the client sent it.
        config.services.nginx.virtualHosts."homepage-pages" = {
          listen = [
            {
              addr = "127.0.0.1";
              inherit port;
            }
          ];
          # The only server on this socket, so also its default server: the Host
          # that arrives is the published name the gate kept, not this one.
          default = true;
          locations."/" = {
            proxyPass = "http://$homepage_upstream";
            # next.js -- the dev-ish HMR channel and the widget event stream.
            proxyWebsockets = true;
          };
        };
      };
  };
}
