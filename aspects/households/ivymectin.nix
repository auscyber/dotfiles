{ lib, ... }:
let
  # ── WHAT EVERY RESIDENT GETS, WRITTEN ONCE ──
  #
  # The house's shared apps: the chores list, the receipts, the bill splitting,
  # the recipes, the shopping, the video library. Stated here and merged into
  # every member below rather than repeated four times, so adding a resident
  # adds these without anyone remembering to.
  #
  # The level is each service's own non-admin level, taken from its
  # `gives.service.access` -- `splitpro` declares only `member` and `grocy` and
  # `paperless` only `user`, so these are not a style choice. A member who
  # should be more than this says so on their own entry, which is what ivy's
  # `admin` grants below are.
  # `mkDefault`, not a plain value: this is the FALLBACK level, and a member
  # who should have more states it on their own entry and wins on priority.
  # ../../CLAUDE.md -- a fallback belongs in a `default`, not in an `if` or a
  # `//` that silently decides which side loses.
  everyone = lib.mapAttrs (_: level: { access = lib.mkDefault level; }) {
    donetick = "user";
    grocy = "user";
    paperless = "user";
    splitpro = "member";
    tandoor = "user";
    plex = "user";
  };
in
{
  # One file per household. The schema is ../base/household.nix; this is data.
  #
  # Members are keyed by PERSON name and derive from ../persons/<name>.nix, so
  # a member here is a person's presence in this house, not a second copy of
  # the human. Removing a member removes their rooms and everything scoped to
  # them, and nothing else.
  den.households.ivymectin = {
    displayName = "Ivymectin";

    # Rooms belong to the place.
    #
    # THIS USED TO CLAIM the rooms line up with the wifi SSIDs, and it did not:
    # ../network/house-wifi.nix runs `hallway` and `bedroom`, while the rooms
    # below are hallway (which IS shared, so "the two non-shared ones" named the
    # wrong pair) and TWO bedrooms, one per person. house-wifi.nix was written to
    # remove exactly that prose duplicate and cites this comment as the thing it
    # replaces; the comment outlived the fact and went stale. The SSIDs live
    # there, the rooms live here, and they are not the same list.
    rooms = {
      hallway = {
        displayName = "Hallway";
        floor = "ground";
        shared = true;
      };
      kitchen = {
        displayName = "Kitchen";
        floor = "ground";
        shared = true;
      };
      lounge = {
        displayName = "Lounge";
        floor = "ground";
        shared = true;
      };
      # Keyed by whose room it is, not just what kind of room it is. There
      # will be more than one bedroom, and a room's NAME is what shows up in
      # Home Assistant and in every access decision -- "Bedroom" would be
      # ambiguous the moment a second person is a member.
      "ivys-bedroom" = {
        displayName = "Ivy's Bedroom";
        floor = "first";
      };
      "jonesys-bedroom" = {
        displayName = "Jonesy's Bedroom";
        floor = "first";
      };
    };

    # What the house PROVIDES. A host claims the ones it serves with
    # `services.<n>.enable = true` (../base/service.nix), so moving a service
    # between machines does not touch this block.
    #
    # ── WHAT IS NOT HERE ANY MORE: PORTS AND `reach` ──
    #
    # Both were written here AND on the aspect that implements each service,
    # and the aspect's copy is the one with readers -- ../base/firewall.nix
    # derives the firewall from `gives`, ../base/capability.nix resolves routes
    # against `gives` and `reach`. This file's copy had none at all. So they
    # are READ-ONLY VIEWS on the service now (../../lib/service-type.nix), and
    # the facts live where the listening is done:
    #
    #   port       `gives.tcp`/`gives.udp`, or `takes.gated.port` for a service
    #              whose only caller is the proxy in front of it
    #   reach      on the aspect, where "may another box borrow mine" is a
    #              question about the server rather than about the asker
    #
    # `nix eval .#debug.config --apply` over `den.households.ivymectin.services`
    # still prints both; they just cannot disagree with the running config.
    #
    # Groups of services granted together. A service joins one with `group`;
    # the levels live here because a tag is a string with nowhere to put them.
    serviceGroups.arr = {
      displayName = "Media automation";
      access = {
        levels = [
          "user"
          "admin"
        ];
        defaultLevel = "user";
      };
    };

    services = {
      # Postgres (../services/postgres.nix) and garage (../services/garage.nix)
      # are both `reach = "host"` on their own aspects, which is the fact that
      # matters about them here: stateful storage whose contents are the whole
      # point, so another machine borrowing this one's is exactly the failure
      # that option exists to prevent.
      postgres = { };
      garage = { };

      # The media automation stack, granted as one. Five of the six are
      # published under `arr.ivymect.in/<path>` via each app's own urlbase;
      # qbittorrent stays on its own subdomain because its WebUI serves
      # absolute paths and has no urlbase to set. The `group` tag is about
      # ACCESS, not publishing, so that difference does not matter here.
      # The media automation stack. Each app TAGS ITSELF `arr`
      # (`gives.service.group`, ../services/media/_servarr.nix), so one grant
      # below covers all six; the LEVELS that grant carries are this house's
      # and stay in `serviceGroups` above.
      sonarr = { };
      radarr = { };
      lidarr = { };
      prowlarr = { };
      bazarr = { };
      qbittorrent = { };

      # ── NO `access` BLOCKS ANY MORE ──
      #
      # What a grant means is the service's own fact and is given by the
      # aspect that implements it (`gives.service.access`): vikunja's levels
      # are vikunja's, donetick maps `admin_groups` in its own config,
      # splitpro has no role model at all. This file used to state all three,
      # which is the house describing permission models it does not own --
      # and ../../lib/service-type.nix's own description already said the
      # aspect fills them in. The household CONSUMES that character now.
      vikunja = { };
      donetick = { };
      splitpro = { };

      # Stock, shopping lists, recipes and batteries -- the house's own
      # inventory (../services/grocy.nix). It subsumes neither vikunja nor
      # donetick: its chores and tasks are off by feature flag, because a
      # grocy assignment names a row in grocy's own sqlite and theirs name a
      # kanidm person.
      grocy = { };

      # Recipes, meal plans and the shopping list they produce
      # (../services/tandoor-recipes.nix). It OVERLAPS grocy, which also has
      # recipes and a meal plan, and the overlap is unresolved: grocy's
      # `FEATURE_FLAG_RECIPES` (its meal plan hangs off the same feature) is
      # the switch that would settle it the way its chores and tasks were
      # settled, and nothing has flipped it yet. There is no Tandoor-to-grocy
      # sync to lean on either -- Tandoor 2.6.13 has no grocy integration at
      # all, and its only shopping-list connector is Home Assistant -- so the
      # two inventories do not know about each other and the duplication is
      # real rather than papered over.
      tandoor = { };

      # Scanned paper: receipts, invoices, statements, OCR'd and searchable
      # (../services/paperless.nix). It does NOT overlap grocy or tandoor --
      # paperless holds the DOCUMENT, grocy holds the stock the document is
      # evidence for -- and the thing that would join the two is a receipt
      # line-item parser that does not exist in either. See
      # ../../docs/receipts-and-expenses.md.
      paperless = { };

      audiobookshelf = { };

      # NO `access` BLOCK, and that is deliberate rather than an omission.
      # Two independent reasons:
      #
      #   * Declaring one makes ../services/access.nix write
      #     `gateway.services.nextcloud.extraGroups` as soon as a host claims
      #     this service -- which CREATES a gateway entry, whose `upstream` has
      #     no default (../services/gateway.nix), so the host stops evaluating
      #     with "option used but not defined". Nextcloud is published on its
      #     own vhost outside the gated machinery on purpose: sync, CalDAV and
      #     Zotero clients cannot follow an SSO redirect.
      #   * Nextcloud's only role is membership of the hardcoded `admin` group,
      #     and the one declarative path into a Nextcloud group is user_oidc
      #     provisioning it from a claim of the same NAME -- so an
      #     `app-nextcloud-admin` group could never reach it. There is nothing
      #     for a level to mean.
      #
      # A member still REQUESTS it (`services.nextcloud = { }` below): with
      # `access` null, ../services/access.nix skips the service entirely, so the
      # request is a statement of who uses it and emits no group.
      #
      # Its `endpoints` view is empty, which is also correct rather than
      # missing: nextcloud is nginx + php-fpm over a unix socket, so the only
      # port in play is the vhost's 443 and that belongs to the proxy.
      nextcloud = { };

      # ../services/homeassistant/music-assistant.nix. Two listeners, the http
      # API and the VBAN receiver auspc's turntable streams to -- both on the
      # aspect, which is also where the receiver's `expose` lives, since the
      # firewall is derived from it.
      music-assistant = { };

      # The house's own mail (../services/mail.nix). No `access` block: a
      # mailbox is not a level in a service, it is an address a person holds,
      # and who has one is pushed from the person rather than granted here.
      mail = { };

      # The house's video library. Implemented by ../hosts/secondpc/plex.nix,
      # found by what that aspect GIVES rather than by this name -- its
      # `gives.service.access` is where the levels live.
      plex = { };

      # NAMED BY THE HOUSEHOLD, IMPLEMENTED BY `homeassistant`. Nothing is
      # written here to connect them: that aspect FURNISHES the service name
      # (`gives.service.name = "home-assistant"`), so the provider lookup in
      # ../../lib/service-type.nix finds it by what it gives rather than by
      # what it is called.
      home-assistant = { };
    };

    # KEYED BY PERSON, which is `ivy` and not `auscyber`: a member is a
    # person's presence in this house, and the person is the human
    # (../persons/ivy.nix). The kanidm groups these grants produce still name
    # the ACCOUNT -- ../services/access.nix reads `person.kanidm.name` for
    # exactly that reason.
    # ── THE MEMBERS ──
    #
    # `mkMerge`, because there are two sources: the baseline every resident
    # gets, and what each person has beyond it. `members` is
    # `attrsOf (submodule ...)` so the module system merges the two per person
    # and the `mkDefault` in `everyone` loses to any level stated below --
    # which is the whole reason the baseline is expressed as a default rather
    # than composed with `//`.
    members = lib.mkMerge [
      # Everyone who lives here, including anyone added later: adding a name to
      # this list is all it takes for them to get the shared apps.
      (lib.genAttrs
        [
          "des"
          "ivy"
          "jonesy"
          "leliana"
        ]
        (_: {
          services = everyone;
        })
      )

      {
        ivy = {
          # Shared rooms are implicit; this is what is specifically theirs, and so
          # what other members' access does NOT extend to.
          rooms = [ "ivys-bedroom" ];

          # What they may do HERE, BEYOND the baseline every resident gets. The
          # baseline arrives through the `mkMerge` below, so there is no `//` and
          # no second copy of it: these are plain definitions and a plain
          # definition beats the baseline's `mkDefault`.
          #
          # Validated against each service's own `access` quirk, so a typo in
          # either half is an evaluation error.
          services = {
            vikunja.access = "admin";
            donetick.access = "admin";

            # Tandoor's own levels (../services/tandoor-recipes.nix). `admin` is
            # the kanidm group existing, not a Tandoor role: nothing maps a claim
            # onto Tandoor's per-space groups yet, so inside the app the first
            # person to sign in owns the space and everyone after joins it as
            # `SOCIAL_DEFAULT_GROUP`.
            tandoor.access = "admin";

            # No level: nextcloud declares no `access`, so the presence of the
            # attribute is the whole request. See the service above.
            nextcloud = { };

            # One grant over every service tagged `arr` -- sonarr, radarr, lidarr,
            # prowlarr, bazarr and qbittorrent -- instead of six.
            arr.access = "admin";

            # The house's video library. `admin` is the server owner; the level is
            # a kanidm group and not a Plex role -- see the note on
            # `gives.service.access` in ../hosts/secondpc/plex.nix.
            plex.access = "admin";

            # States that she uses the house's mail. The MAILBOX itself comes from
            # ../persons/ivy.nix's `den.aspects.mailbox` include, which is what
            # reaches the serving host; this is the household-side record of who
            # uses what, the same as `nextcloud` above.
            mail = { };
          };
        };

        # Their own room, which is why it is declared above.
        jonesy.rooms = [ "jonesys-bedroom" ];
      }
    ];

    # ── THE OTHER THREE RESIDENTS ──
    #
    # `den.persons` has four people and this household had ONE membership, so
    # three of them propagated nothing: no rooms, no `reachableRooms`, and no
    # presence in `_module.args.memberships` -- which is the list every
    # household-scoped query walks. The tell that it was an omission rather
    # than a choice is `rooms.jonesys-bedroom` above: a room named for someone
    # with no membership to reach it through.
    #
    # `guest` is left at its default (false): all three are resident, which is
    # the distinction the option exists to draw. Only jonesy has a room of
    # their own; the others keep `rooms = [ ]`, and shared rooms are implicit
    # for every member, so listing none is not the same as being a guest.
  };
}
