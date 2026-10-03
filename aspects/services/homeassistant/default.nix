{ den, ... }:
# Home Assistant, native on the host per https://wiki.nixos.org/wiki/Home_Assistant.
#
# This used to be HAOS as a NixVirt-declared libvirt domain (`_haos.nix` plus
# `_bridged-network.nix` / `_default-network.nix`, all removed), with the
# supervisor owning mosquitto, matter-server, esphome and Music Assistant as
# add-ons inside the VM. Native HA has no supervisor, so each of those is a host
# service here and the `hassio` integration simply ceases to exist.
#
# The integration list below is what the HAOS instance actually had -- read off
# its `.storage/core.config_entries` -- minus the two that cannot survive the
# move: `hassio` (supervisor) and `hacs` (installs components imperatively into
# the config dir, which is what `customComponents` replaces).
let
  haPort = 8123;
  # HA's HomeKit bridge serves its own port, independent of the HTTP frontend;
  # the HAOS instance was already on 21064 and the iOS Home app has that paired.
  homekitPort = 21064;
  mqttPort = 1883;
  esphomePort = 6052;
  matterPort = 5580;
  # Music Assistant's webserver. Not an option on the nixpkgs module -- it is
  # the server's own default, and the module passes no --port.
  musicAssistantPort = 8095;

  domain = "ivymect.in";
  issuer = "https://auth.${domain}/oauth2/openid/home-assistant";
in
{
  den.aspects.homeassistant = {
    includes = [
      # nginx + ACME (wildcard ivymect.in) + the cloudflare tunnel that already
      # fronts every other *.ivymect.in vhost on this box.
      den.aspects.secondpc-web
      # `gated`, for the SSO-fronted esphome and Music Assistant vhosts.
      den.aspects.gateway
    ];

    # Direct TLS, not behind the shared oauth2-proxy SSO (../../hosts/secondpc/sso.nix)
    # that gates grafana/homepage/etc: HA has its own login -- kanidm-backed via
    # auth_oidc below -- and the companion app plus any voice-assistant
    # integration cannot follow an interactive SSO redirect.
    vhosts."home.${domain}" = {
      useACMEHost = domain;
      forceSSL = true;
      # The wiki's one nginx-side requirement. HA streams (camera, logbook
      # subscriptions) through this vhost and buffering them breaks playback.
      extraConfig = ''
        proxy_buffering off;
      '';
      locations."/" = {
        proxyPass = "http://127.0.0.1:${toString haPort}";
        proxyWebsockets = true;
      };
    };

    # ESPHome's dashboard and Music Assistant's UI were supervisor ingress
    # panels, reachable only through HA's own session. Native they are plain web
    # apps with no auth of their own, so they go behind oauth2-proxy.
    gated = {
      esphome = {
        upstream = "http://127.0.0.1:${toString esphomePort}";
        websockets = true;
      };
      music-assistant = {
        upstream = "http://127.0.0.1:${toString musicAssistantPort}";
        websockets = true;
      };
    };

    nixos = { pkgs, ... }: {
      # A PUBLIC client (PKCE, no secret): hass-oidc-auth defaults to public
      # and kanidm enforces PKCE for one, so there is no client secret to
      # generate, rekey, or get into a store-rendered configuration.yaml.
      #
      # Written through `nixos` rather than the `provision` class
      # (../gateway.nix) that ../../hosts/secondpc/sso.nix uses, even though
      # that class exists for exactly this. Class content from two aspects is
      # merged one level deep, so a second aspect writing `systems` replaces
      # the first aspect's whole `systems` tree -- which silently dropped the
      # `oauth2-proxy` client and took every SSO-gated vhost with it. Going
      # through the NixOS option system instead merges `systems.oauth2` by
      # attribute, which is what was wanted.
      services.kanidm.provision.systems.oauth2.home-assistant = {
        displayName = "Home Assistant";
        originUrl = "https://home.${domain}/auth/oidc/callback";
        originLanding = "https://home.${domain}/";
        public = true;
        preferShortUsername = true;
        # `groups_name`, not `groups`: only that form yields bare names like
        # `media-users`, which is what auth_oidc's `roles` below match
        # against. The SPN forms arrive as `media-users@auth.ivymect.in` and
        # never match.
        scopeMaps.media-users = [
          "openid"
          "profile"
          "email"
          "groups_name"
        ];
      };

      services.home-assistant = {
        enable = true;

        # Every integration the HAOS instance had a config entry for, plus
        # the wiki's baseline. `extraComponents` is what pulls each one's
        # python dependencies into the package -- the module also infers them
        # from `config` keys, but almost all of these are UI config entries
        # with no YAML presence, so they have to be named.
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

          # Local protocols and hubs.
          "esphome"
          "mqtt"
          "matter"
          "thread"
          "music_assistant"

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
        ];

        # Recorder on postgres instead of the bundled sqlite.
        extraPackages = python3Packages: with python3Packages; [ psycopg2 ];

        # auth_oidc is the component HACS used to install into the config dir
        # (`custom_components/auth_oidc`, v1.2.1 there); nixpkgs packages the
        # same release, so it is declared rather than fetched at runtime.
        customComponents = with pkgs.home-assistant-custom-components; [
          auth_oidc
        ];

        config = {
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

          # kanidm as an additional login provider alongside HA's own, which
          # stays enabled so there is still a way in if the IdP is down.
          auth_oidc = {
            client_id = "home-assistant";
            discovery_url = "${issuer}/.well-known/openid-configuration";
            display_name = "kanidm";
            # kanidm names the SCOPE `groups_name`; the CLAIM it lands in is
            # the plain `groups` auth_oidc already reads by default.
            groups_scope = "groups_name";
            features = {
              # Match an OIDC login onto the HA user of the same name, so the
              # existing `auscyber` account keeps its dashboards and tokens
              # rather than a parallel one appearing.
              automatic_user_linking = true;
              automatic_person_creation = true;
              # nginx terminates TLS, so HA's own view of the request is
              # http; without this the URLs it hands the IdP are too.
              force_https = true;
            };
            roles = {
              admin = "idm_admins";
              user = "media-users";
            };
          };
        };
      };

      # --- recorder backend ---
      services.postgresql = {
        enable = true;
        ensureDatabases = [ "hass" ];
        ensureUsers = [
          {
            name = "hass";
            ensureDBOwnership = true;
          }
        ];
      };

      # `postgresql-setup` is the unit that runs ensureDatabases/ensureUsers,
      # and it is a separate oneshot from `postgresql` itself -- ordering
      # against the latter alone lets HA reach a live server that has no
      # `hass` database yet.
      systemd.services.home-assistant = {
        after = [ "postgresql-setup.service" ];
        requires = [ "postgresql-setup.service" ];
      };

      # --- what used to be the mosquitto add-on ---
      #
      # On the LAN, not loopback: the ESPHome nodes publish to it.
      #
      # The hashes live OUTSIDE nix, at the paths below, rather than as
      # agenix-generated secrets: a generated secret has to be minted with
      # the gpg-yubikey master identity before the host will even evaluate,
      # and these two passwords are typed into HA's MQTT config entry and the
      # ESPHome secrets by hand anyway -- exactly as they were in the add-on's
      # own options. So they are host state, set once per broker:
      #
      #   umask 077
      #   for u in homeassistant sendspin; do
      #     mosquitto_passwd -c -b /tmp/mq "$u" "<password>"
      #     cut -d: -f2- /tmp/mq > /var/lib/mosquitto-passwords/"$u"
      #   done
      #   rm -f /tmp/mq
      #
      # One bare hash per file, no `user:` prefix -- that is the format the
      # module's `hashedPasswordFile` wants, and it fails the unit rather than
      # starting open if a file is missing.
      #
      # `acl` is not optional: the module renders a user with an empty acl as
      # a `user <name>` stanza with no topics, which denies everything.
      systemd.tmpfiles.settings."10-mosquitto-passwords"."/var/lib/mosquitto-passwords".d = {
        user = "root";
        group = "root";
        mode = "0700";
      };

      services.mosquitto = {
        enable = true;
        listeners = [
          {
            address = "0.0.0.0";
            port = mqttPort;
            users = {
              homeassistant = {
                acl = [ "readwrite #" ];
                hashedPasswordFile = "/var/lib/mosquitto-passwords/homeassistant";
              };
              # Carried over from the add-on's own login list.
              sendspin = {
                acl = [ "readwrite #" ];
                hashedPasswordFile = "/var/lib/mosquitto-passwords/sendspin";
              };
            };
          }
        ];
      };

      # --- what used to be the matter-server add-on ---
      services.matter-server = {
        enable = true;
        port = matterPort;
        # logLevel = "debug";
      };

      # --- what used to be the esphome add-on ---
      #
      # Loopback only; the `gated` entry above is the way in. Device configs
      # live in its state dir, which is where the add-on's own copies go.
      services.esphome = {
        enable = true;
        address = "127.0.0.1";
        port = esphomePort;
      };

      # --- what used to be the Music Assistant add-on ---
      #
      # `providers` names only the ones with extra dependencies to install;
      # the module folds in the package's builtins itself. This list is the
      # add-on's enabled providers minus those builtins.
      services.music-assistant = {
        enable = true;
        openFirewall = true;
        providers = [
          "hass"
          "airplay"
          "airplay_receiver"
          "chromecast"
          "dlna"
          "sonos"
          "universal_group"
          "vban_receiver"
          "filesystem_local"
          "tidal"
          "lastfm_recommendations"
          "lastfm_scrobble"
          "ambient_sounds"
          "party"
          "smart_fades"
          "milkdrop_visualizer"
        ];
      };

      networking.firewall = {
        allowedTCPPorts = [
          haPort
          mqttPort
          homekitPort
        ];
        allowedUDPPorts = [
          # mDNS: zeroconf discovery, and how the Home app finds the HomeKit
          # bridge. HA uses python-zeroconf in-process, so this is the only
          # thing it needs from the host.
          5353
          # Matter commissioning/operational.
          5540
          # SSDP, for the upnp and dlna_dms integrations.
          1900
          # VBAN, for Music Assistant's vban_receiver provider -- the
          # protocol's own default port, and the one auspc's emitter sends
          # to. `openFirewall` on the module does not cover this provider.
          6980
        ];
      };
    };
  };
}
