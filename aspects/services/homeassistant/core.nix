{
  den,
  lib,
  caps,
  ...
}:
# The Home Assistant service itself: which integrations are present, where the
# recorder writes, what the UI editors are still allowed to own, and the ports
# HA answers on. ./default.nix has the history of this directory and the index
# of its files.
{
  # ── WHAT OTHER SERVICES PUSH INTO HOME ASSISTANT ──
  #
  # Declared here, in the aspect that IS Home Assistant, rather than in
  # ../../base/capability-types.nix with everything else: ownership and editing
  # become odd when the definition of "what can be pushed into HA" lives in a
  # file that knows nothing about HA.
  den.capabilities.home-assistant = {
    meta.description = "Home Assistant, which other services and people push configuration into.";
    includes = [ caps.carried ];

    taker.options = {
      # ── ONE PYTHON ENVIRONMENT ──
      #
      # Every taker's components go into one `extraComponents` list, and two
      # takers naming the same one (`default_config` pulls in plenty) must not
      # produce it twice -- which is `listOf` plus `apply`.
      extraComponents = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ ];
        apply = lib.unique;
      };

      # Anything else a taker wants folded into HA's own config. Raw, because
      # HA's config surface is the whole of `services.home-assistant.config`
      # and enumerating it here would be a second copy of nixpkgs' module.
      config = lib.mkOption {
        type = lib.types.attrsOf lib.types.anything;
        default = { };
      };
    };

    takerMerged = [ "extraComponents" ];
  };

  den.aspects.homeassistant.nixos =
    {
      host ? { },
      ...
    }:
    let
      # ── THE CONTRIBUTIONS, WHICH NOTHING READ ──
      #
      # This is the half that was missing, and it was missing silently. Three
      # aspects declare `takes.home-assistant.extraComponents` -- mosquitto's
      # `mqtt`, matter-server's `matter`, Music Assistant's `music_assistant`
      # -- and each of them was removed from the hand-written list below on the
      # reasoning that "the aspect that provides the service contributes its
      # own component". The contributions were real (the resolver returns all
      # three) but NO ONE READ THEM, so the components were simply gone: HA
      # would have come up with no MQTT, no Matter and no Music Assistant
      # integration, with every file involved looking correct.
      #
      # A take is only half a wiring. The giver has to read it -- the same
      # thing ../postgres.nix does for databases and ./mosquitto.nix now does
      # for broker logins.
      # ── REACHING, NOT LOCAL ──
      #
      # `den.lib.capability.contributions` asks `takersOn host` -- takers whose aspect
      # is on THIS box. Every taker in this directory is, so that worked by
      # accident until ../openrgb.nix: auspc's lighting daemon, which this
      # service integrates with across the house. Its component resolved and
      # reached nobody, which is the SAME failure the note above describes --
      # contributions that exist and nothing reads -- arrived at from the other
      # direction.
      #
      # `reach = "household"` below is what decides how far this looks, so the
      # giver is passed rather than only the host. See `contributionsReaching`
      # in ../../base/capability.nix.
      contributed =
        den.lib.capability.contributionsReaching den.aspects.homeassistant "home-assistant"
          host;
    in
    {
      services.home-assistant = {
        enable = true;

        # Every integration the HAOS instance had a config entry for, plus
        # the wiki's baseline. `extraComponents` is what pulls each one's
        # python dependencies into the package -- the module also infers them
        # from `config` keys, but almost all of these are UI config entries
        # with no YAML presence, so they have to be named.
        #
        # THE BASELINE, PLUS WHAT WAS ASKED FOR. A service that HA integrates
        # with declares the integration itself, so removing the service removes
        # its component with it rather than leaving a dangling entry here.
        extraComponents = [
          # Wiki baseline / onboarding.
          "default_config"
          "analytics"
          "google_translate"
          "met"
          "radio_browser"
          "shopping_list"
          # C zlib/crc32 for the websocket API -- cheap and the wiki asks for it.
          "isal"

          # Devices and media on the LAN.
          "apple_tv"
          "dlna_dms"
          "ipp"
          "plex"
          "upnp"
          "wiz"

          # Exposed BY home-assistant rather than consumed.
          "homekit"

          # Pulled in by default_config, named so the module puts go2rtc on
          # the unit's PATH and the mobile app's push setup is present.
          "go2rtc"
          "mobile_app"
          "backup"
          "sun"
        ]
        ++ contributed.extraComponents;

        # Recorder on postgres instead of the bundled sqlite.
        extraPackages = python3Packages: with python3Packages; [ psycopg2 ];

        # `mkMerge` and not `//`: `config` is contributed per taker (`merge =
        # "each"`), so two takers pushing different keys both land, and a taker
        # pushing a key this file also sets is a module-system conflict naming
        # both rather than one silently winning.
        config = lib.mkMerge (
          lib.attrValues contributed.config
          ++ [
            {
              # Do not remove -- this is the integration set HA onboards with.
              default_config = { };

              # Verbatim from the HAOS instance's configuration.yaml. The `!`
              # forms survive YAML rendering (the module unquotes them), and
              # keeping them is what leaves the UI's automation/script/scene
              # editors working: those files stay writable in the config dir
              # while configuration.yaml itself is a store symlink.
              frontend.themes = "!include_dir_merge_named themes";
              automation = "!include automations.yaml";
              script = "!include scripts.yaml";
              scene = "!include scenes.yaml";

              # NO `person:` BLOCK, and that is deliberate rather than an omission.
              # ./auth.nix sets `auth_oidc.features.automatic_person_creation`, so a
              # kanidm login creates the HA person on its own and the registry in
              # .storage is the only copy there is (./state.nix's note on `.storage`
              # says why that matters). A `person:` key here would declare a SECOND,
              # YAML-owned person list that HA shows alongside the created ones and
              # that cannot be edited in the UI.

              # Left bound to every interface, as it was in the VM: the companion
              # app, HomeKit, and anything fetching TTS or camera media off HA
              # reach it on the LAN, not through the tunnel. `trusted_proxies` is
              # therefore the loopback nginx leg only -- the wiki's stricter
              # `server_host = "::1"` would take LAN access with it.
              http = {
                use_x_forwarded_for = true;
                trusted_proxies = [
                  "127.0.0.1"
                  "::1"
                ];
              };

              # Peer auth over the unix socket, so there is no password anywhere.
              # `host=` is explicit because psycopg2 otherwise falls back to its
              # compiled-in socket directory, which is not nixpkgs'.
              recorder.db_url = "postgresql://@/hass?host=/run/postgresql";
            }
          ]
        );
      };

      # ── NO `networking.firewall` HERE ANY MORE ──
      #
      # It listed `haPort`, `homekitPort`, 5353 and 1900 -- the same four ports
      # the `gives` below declares, which is the duplication this aspect's own
      # comment already flagged ("`expose` is what turns a port into a firewall
      # rule"). They were both live because the derived firewall was opening
      # NOTHING: ../../base/firewall.nix was asking the projected host entity,
      # which answers no to `hasAspect`, so every rule here was the only one
      # that existed and the `gives` were decoration.
      #
      # Deleting them TIGHTENS the result rather than preserving it, which is
      # the point. `allowedTCPPorts` is nixpkgs' pseudo-interface `default` and
      # its rules carry no `-i`, so 8123 was open on every wire this box has;
      # now each port is open on exactly the carriers it names.
    };

  # ── HOME ASSISTANT AS A SERVICE, AND THE COLLECTOR FOR WHAT IS PUSHED INTO IT ──
  #
  # Anything that integrates with HA contributes its own component through
  # `uses.home-assistant.giving`, and this folds every taker's contribution --
  # the same provider-reads-its-dependents shape ../postgres.nix has for
  # databases and ../shares.nix has for folders, but with a wanter identity,
  # which a quirk pool does not carry.
  #
  # That is also what lets a PERSON push into a service: §3c's three wanter
  # kinds are person, host and service, so
  # `den.persons.ivy.uses.home-assistant.giving = { ... }` is the same relation
  # with a different asker, and HomeKit configured by one person lands here
  # without anything in this file knowing who they are.
  #
  # THE UDP PORTS ARE EDGES, not literals. mDNS, SSDP and Matter commissioning
  # are declared as `uses.udp.shared.ports`, and ../../base/service-implement.nix
  # turns them into firewall rules -- so the port is stated once by the thing
  # that listens on it, and the hole lasts exactly as long as the service does.
  # ── WHAT HOME ASSISTANT LISTENS ON, as capabilities it gives ──
  #
  # Was `den.serviceTypes.home-assistant` with `uses.tcp.shared.port`. It is a
  # GIVE, not a use: the port is what this service offers, and
  # ../../base/capability-types.nix puts it on the link because the server
  # listens on it and the consumer dials it -- one fact, not two.
  #
  # `expose` is what turns a port into a firewall rule, so the rules are DERIVED
  # from what services actually offer instead of being hand-added. 8123 stays
  # closed because it is behind nginx and opening it would route around the
  # gate; the discovery protocols are useless unless the LAN can reach them.
  # A HOUSE AMENITY, not fleet infrastructure and not host-local: every phone
  # and every box in this household talks to it, and nothing outside does.
  # ../households/ivymectin.nix used to carry this instead, where it was the
  # same fact written twice and able to disagree with the aspect that actually
  # resolves takes.
  # ── IT IS A BROKER CLIENT, AND SAYS SO ──
  #
  # ./mosquitto.nix derives its login list from whoever takes `mqtt`, so this
  # is what gets Home Assistant an account -- and the account disappears if
  # this line does. The default acl is `readwrite #`, which is what the
  # hand-written user already had.
  den.aspects.homeassistant.takes.mqtt = { };

  # SPEAKS BOTH GROUPS, so it includes them rather than declaring their
  # ports -- see the note where the `udp` giver used to be.
  den.aspects.homeassistant.includes = [
    den.aspects.mdns
    den.aspects.ssdp
  ];

  den.aspects.homeassistant.reach = "household";

  den.aspects.homeassistant.gives =
    { self, ... }:
    {
      # A SERVICE THE HOUSE OFFERS, under a name that is NOT this aspect's.
      # The aspect is `homeassistant` and the service is `home-assistant`;
      # every grant, kanidm group and member request already spells it the
      # second way, so the giver furnishes it rather than letting the derived
      # view rename them all. `name` is `carried`'s field, which exists for
      # exactly this -- "which instance this is, by the name its giver
      # furnishes".
      #
      # No `access`: HA maps OIDC groups to its own roles through a config
      # this aspect does not write, so a level here would name nothing kanidm
      # could reach.
      service = _: {
        name = "home-assistant";
      };

      # ── 8123 AND THE HOMEKIT BRIDGE ──
      #
      # `expose` MATCHES WHAT THIS HOST DOES TODAY, and it contradicts
      # what the old `den.serviceTypes.home-assistant` said about itself: that
      # comment claimed 8123 stays closed because "opening it would route
      # around the gate", while the live `nixos` body in this same file has
      # always had `allowedTCPPorts = [ haPort homekitPort ]`.
      #
      # So the declaration and the config disagreed, and the config won because
      # it is the half that runs. Preserved as-is rather than resolved here --
      # closing 8123 is a deliberate change with a blast radius (anything on the
      # LAN talking to HA directly, not through nginx), and it should be made
      # on purpose rather than as a side effect of a refactor.
      # NAMED, so a consumer can target one. `ports.homekit` is what the iOS
      # Home app paired against and `ports.http` is what nginx proxies -- a list
      # made those indistinguishable, so anything wanting the bridge had to know
      # it was the second element.
      tcp = _: {
        # NAMED, so a consumer can dial THIS service rather than "whatever
        # gives tcp on secondpc" -- which is twenty other things. The name is
        # `home-assistant` and not this aspect's own `homeassistant`, matching
        # what `gives.service` furnishes and what every grant already spells.
        name = "home-assistant";

        ports = {
          # EXPOSED ON THE HOUSE LAN -- `br0` on this box, resolved from the
          # carrier rather than named here. It is the LAN and not the tailnet
          # because the iOS Home app and every ESPHome device are on the
          # former. Matching what this host does today -- and it contradicts
          # what the old serviceType claimed about itself ("opening it would
          # route around the gate") while the live `nixos` body here has always
          # had `allowedTCPPorts = [ haPort homekitPort ]`. The config won
          # because it is the half that runs; closing 8123 is a deliberate
          # change with a blast radius, not a refactor side effect.
          http = {
            # 8123 LIVES HERE, which is the only place it now appears. It was
            # in ./_ports.nix, a file of shared constants read with
            # `import ./_ports.nix` -- a path import of one of our own
            # helpers, which ../../../CLAUDE.md's first rule forbids, and a
            # second place for a number this aspect already declares to the
            # capability layer. Anything that needs it RESOLVES it.
            port = 8123;
            meta.description = "Home Assistant's own web UI and REST/websocket API: nginx proxies it, and the phones dial it directly off the lan or the tailnet.";
            # THE LAN AND THE TAILNET. The lan is where the phones are; the
            # tailnet is here because the global rule this replaces WAS
            # reaching `tailscale0` -- it matched every interface -- and
            # narrowing to the lan alone would revoke off-LAN access as a side
            # effect of a refactor, which is the thing this file's own header
            # refuses to do to 8123.
            expose = [
              "lan-home"
              "tailscale"
            ];
          };
          # The HomeKit bridge serves its own port, independent of the HTTP
          # frontend; the iOS Home app has 21064 paired and must reach it from
          # the LAN.
          homekit = {
            port = 21064;
            meta.description = "The HomeKit bridge's accessory protocol, which the iOS Home app has paired against and reaches from the lan.";
            expose = [ "lan-home" ];
          };
        };
        carrier = self.name;
      };

      # ── NO `udp` GIVER: 5353 AND 1900 BELONG TO THEIR GROUPS ──
      # This declared both, and because a socket is exclusive per host it
      # was then the ONLY aspect allowed to -- so avahi's and the media
      # server's mDNS holes came from here by accident. The groups own them
      # now (../../network/multicast.nix) and this aspect includes them,
      # which says what is true: it is a speaker, not the owner.

      # The account it runs as -- `hass`, NOT `homeassistant`, which is the
      # aspect's name. ../postgres.nix resolves this for the database and role
      # names, because peer auth needs the role to equal the connecting user.
      user = _: {
        name = "hass";
        group = "hass";
        carrier = self.name;
      };

      # WHAT OTHER SERVICES PUSH INTO. Giving this is what makes a
      # `takes.home-assistant` resolve -- ./music-assistant.nix's component, and
      # (with a person as the asker) HomeKit configured by one member of the
      # household.
      home-assistant = _: {
        carrier = self.name;
      };
    };
}
