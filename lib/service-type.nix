{
  lib,
  den,
  capability,
}:
# The instance type for a household's nested `services`, built once in
# aspects/base/service-libs.nix and applied per household by
# aspects/base/household.nix -- it has to be applied where the household
# INSTANCE is in scope, because `users` below is derived from that household's
# members, and a type built from the schema alone cannot see them.
#
# The rest of the service model is in aspects/base/service.nix: the schema, the
# per-host `enable` claim, `serves`, and the `serviceHosts` fleet walk.
#
# ── WHAT A HOUSEHOLD DECLARES, AND WHAT IT NO LONGER RESTATES ──
#
# A household says what it PROVIDES, who may use it and how those grants group.
# It does not say what port the thing listens on or how far it may be borrowed:
# both of those are facts about the service's own implementation, the aspect
# already states them to the capability layer (`gives`, `takes.gated.port`,
# `reach`), and that is the copy every consumer actually reads --
# aspects/base/firewall.nix derives the firewall from it and
# aspects/base/capability.nix resolves routes from it.
#
# So `endpoints` and `reach` below are READ-ONLY VIEWS of the aspect's record
# rather than fields a household fills in. They were authored data until they
# were measured: nothing in the tree read either one, while every port in
# `aspects/households/ivymectin.nix` was also written in the aspect that
# listens on it, and every `reach` there was also written on the aspect that
# resolution consults. Two copies of a fact with one reader is the shape that
# goes quietly wrong -- `aspects/base/capability.nix` records an `endpoints`
# key that outlived the option it was typed against and evaluated as untyped
# data with nothing behind it.
household:
let
  inherit (lib) mkOption types;

  # ── THE PORTS, READ OFF THE ASPECT ──
  #
  # An aspect states a port in one of two places and both are the capability
  # layer: `gives.tcp`/`gives.udp` for something that listens on its own
  # account (home-assistant's 8123, music-assistant's VBAN receiver, garage's
  # three), and `takes.gated.port` for a service whose only caller is the
  # proxy in front of it (the *arr apps, vikunja, audiobookshelf, splitpro).
  # Read both, so a service is not required to pick one spelling to be visible
  # here.
  #
  # KEYED BY PROTOCOL FIRST. A flat map would have to merge `tcp` and `udp`
  # port names, and music-assistant has both -- the http API and the VBAN
  # receiver -- so one collision would silently drop a listener from this view.
  #
  # `host = { }`: this is a view of the SERVICE, which has no host of its own
  # until one claims it, and every `gives` in the tree takes `{ self, ... }`.
  # A giver that needed the host would be skipped by `canTake` rather than
  # given a wrong one.
  # ── WHO PROVIDES A SERVICE BY THAT NAME ──
  #
  # The household says what it WANTS; this is what finds the thing that
  # provides it. Keyed by the name the provider FURNISHES
  # (`gives.service.name`, falling back to the aspect's own), not by the
  # attribute name the household happened to use -- which is why
  # `home-assistant` needed a hand-written `aspect =` line while the aspect
  # is called `homeassistant`.
  #
  # Built ONCE, not per service: this file's outer scope is evaluated a single
  # time in ../aspects/base/service-libs.nix, so the walk over `den.aspects`
  # happens once for the whole flake rather than sixteen times per household.
  providers =
    let
      gives =
        aspect:
        capability.linkFor aspect "service" {
          host = { };
          peer = { };
        };
    in
    lib.listToAttrs (
      lib.concatLists (
        lib.mapAttrsToList (
          name: aspect:
          let
            l = if (aspect.gives or null) == null then null else gives aspect;
          in
          lib.optional (l != null) (lib.nameValuePair (if l.name != null then l.name else name) aspect)
        ) den.aspects
      )
    );

  # ── THE CHARACTER THE HOUSEHOLD CONSUMES ──
  #
  # `gives.service` on the implementing aspect: what a grant means, and which
  # tag groups it with others. Read through the capability layer rather than
  # off the aspect's raw attrs, so it is typed by
  # ../aspects/base/capability-types.nix like every other furnished value and
  # a misspelling is an error at the giver.
  characterOf =
    aspect: field:
    let
      link =
        if aspect == null then
          null
        else
          capability.linkFor aspect "service" {
            host = { };
            peer = { };
          };
    in
    if link == null then null else link.${field} or null;

  endpointsOf =
    aspect:
    let
      portsOf =
        cap:
        let
          half =
            if aspect == null then
              null
            else
              capability.endpointHalf aspect cap {
                host = { };
                peer = { };
              };
        in
        if half == null then { } else lib.mapAttrs (_: p: { inherit (p) port expose; }) half.ports;

      takes = if aspect == null then { } else aspect.takes or { };

      # The gate dials it here. `gated` narrows `proxy`, so either spelling is
      # the same fact; a service that also GIVES the port wins, because that is
      # the half that actually opens the socket.
      gated = takes.gated.port or takes.proxy.port or null;
    in
    lib.filterAttrs (_: v: v != { }) {
      tcp =
        lib.optionalAttrs (gated != null) {
          http = {
            port = gated;
            expose = false;
          };
        }
        // portsOf "tcp";
      udp = portsOf "udp";
    };
in
den.lib.schema.mkInstanceType {
  strict = true;
  extraModules = [
    (
      { name, config, ... }:
      {
        options = {
          household = mkOption {
            type = types.raw;
            readOnly = true;
            default = household;
            description = "The household that declares this service.";
          };

          # The members who asked for THIS service, already projected -- so no
          # aspect ever writes a filter over the household, which is the line
          # that would get copy-pasted into seven services and then diverge.
          #
          # Derived, in the spirit of `gateway.services.<n>.url` and
          # `den.hosts.<h>.users.<u>.anyUser`: the entity knows its own name and
          # its own household, so it is the only thing that can compute this
          # without being told twice. Direction stays down throughout
          # (household -> member -> person), so none of it recurses.
          users = mkOption {
            type = types.raw;
            readOnly = true;
            default = lib.mapAttrs (_: m: {
              inherit (m) person guest rooms;
              request = m.services.${name};
            }) (lib.filterAttrs (_: m: m.services ? ${name}) household.members);
            description = "This household's members who requested this service, keyed by person name.";
          };

          # Auto-linked by NAME, which is the common case, and null when no
          # aspect matches -- a service whose aspect is named differently
          # (`homeassistant` provides `home-assistant`) or where one aspect
          # provides several services (`homeassistant` also provides
          # `music-assistant`). A null aspect means the host includes it the
          # old way, explicitly, and gets no `{ service, ... }` binding; the
          # link is opt-in rather than a rename forced on every aspect.
          aspect = mkOption {
            type = types.nullOr types.raw;
            default = providers.${name} or den.aspects.${name} or null;
            defaultText = "whichever aspect gives a `service` by this name";
            description = ''
              The aspect that implements this service, resolved in a scope
              bound to this entity.

              FOUND BY WHAT IT GIVES, not by what it is called: an aspect
              furnishing `gives.service` under this name claims it, which is
              how `home-assistant` finds `den.aspects.homeassistant` with
              nothing written down. The attribute-name lookup remains as a
              fallback for an aspect that implements a service without
              declaring one.

              Null when nothing provides it -- the household is then asking
              for something no aspect in this flake offers, which
              `checks.household-service-providers`
              (../aspects/services/access.nix) reports rather than silently
              granting nobody anything. Every consequence of this being null is
              otherwise invisible: `access` and `group` go null with it, so the
              service is dropped from the grant collector and every member
              granted it looks granted while no group is ever created.
            '';
          };

          reach = mkOption {
            type = types.raw;
            readOnly = true;
            default = if config.aspect == null then "host" else config.aspect.reach or "host";
            defaultText = "the implementing aspect's `reach`";
            description = ''
              How far a need for this service may resolve, READ OFF THE ASPECT
              THAT IMPLEMENTS IT (aspects/base/capability-types.nix declares
              it; aspects/base/capability.nix is what enforces it).

              Not writable here, and that is the point. Whether a database may
              be shared is a fact about the database, so the providing aspect
              is the only place that can say it -- and it is already the copy
              resolution reads. A household restating it was the same sentence
              twice: `postgres.reach = "host"` here and on the aspect,
              `nextcloud` household in both, with nothing to notice if the two
              ever stopped agreeing.

              `household` is itself resolved against the HOST's household
              (`den.schema.host.household`), which is what makes this a
              question about distance rather than about ownership: a service
              owned by this house but served on a box that serves no house
              reaches nobody.
            '';
          };

          endpoints = mkOption {
            type = types.raw;
            readOnly = true;
            default = endpointsOf config.aspect;
            defaultText = "the ports the implementing aspect states";
            description = ''
              What this service listens on, as `<protocol>.<name> = { port;
              expose; }` -- derived from the aspect, never authored here. See
              `endpointsOf` above for the two places an aspect states a port
              and why both are read.

              A service with nothing of its own answers `{ }`, and for
              nextcloud that is correct rather than missing: it is nginx +
              php-fpm over a unix socket, so the only port involved is the
              vhost's 443, which belongs to the proxy that owns it.
            '';
          };

          access = mkOption {
            type = types.raw;
            readOnly = true;
            default = characterOf config.aspect "access";
            defaultText = "the implementing aspect's `gives.service.access`";
            description = ''
              What a grant in this service MEANS, READ OFF THE ASPECT that
              implements it (`gives.service.access`).

              Not writable here, and the old description already said why
              without the code agreeing: "filled in by the service's own
              aspect". There is deliberately no central `levels` enum --
              Home Assistant maps groups to roles, donetick has
              `admin_groups`, splitpro has no role model at all and
              audiobookshelf's mapping is not declarative -- so the only
              place that can state it is the thing that implements it. The
              household CONSUMES this character; it does not author it.

              Null for a service with nothing to say about permissions, which
              makes ../aspects/services/access.nix skip it entirely rather
              than emit a group that opens nothing.
            '';
          };

          externallyManaged = mkOption {
            type = types.bool;
            readOnly = true;
            default = characterOf config.aspect "externallyManaged" == true;
            defaultText = "the implementing aspect's `gives.service.externallyManaged`";
            description = ''
              The app keeps its own accounts, so a grant here is the
              household's record of who uses it and not an instruction
              anything in nix can carry out. Read off the aspect.

              `== true` rather than a cast: `characterOf` answers null for a
              service with no implementing aspect, and null is not a bool.
            '';
          };

          group = mkOption {
            type = types.nullOr types.str;
            readOnly = true;
            default = characterOf config.aspect "group";
            defaultText = "the implementing aspect's `gives.service.group`";
            description = ''
              The tag that grants this service alongside others carrying it,
              read off the aspect. The LEVELS such a grant carries stay on the
              household's `serviceGroups`: what a grant means to people is the
              house's fact, while which services one grant reaches is the
              service's own.
            '';
          };
        };
      }
    )
  ];
} den.schema.service
