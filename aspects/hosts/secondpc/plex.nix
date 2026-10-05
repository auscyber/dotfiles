{ den, ... }:
# Plex, standing in for jellyfin on secondpc: same Intel VAAPI render-device
# passthrough and the same /mnt/hdd media library jellyfin had access to,
# fronted at media.ivymect.in instead of jellyfin.pierlot.com.au.
# `jellyfin.nix` is kept around (unincluded) in case we ever switch back.
let
  # Every socket plex listens on, named once. `openFirewall` used to supply
  # these as globals; `gives` below is now the only place they appear.
  sockets = {
    # The server itself: the API, the web UI and direct play.
    http = 32400;
    # Plex Companion (remote control from a phone) and the Roku companion.
    companion = 3005;
    roku = 8324;
    # DLNA: the server's HTTP endpoint plus GDM discovery.
    dlna = 32469;
    gdm = [
      32410
      32412
      32413
      32414
    ];
  };
in
{
  den.aspects.plex = {
    includes = [
      den.aspects.secondpc-web
      den.aspects.gateway
      # Tautulli, declared at the bottom of this file. Here rather than as a
      # separate `den.aspects.plex.includes` line, which would be a second
      # definition of this same attribute in one literal.
      den.aspects.tautulli
    ];

    # TAUTULLI IS DECLARED BELOW, AS ITS OWN ASPECT THAT THIS ONE INCLUDES.
    #
    # It is still plex's business -- it has nothing to do if plex is not here --
    # and including it keeps that true: it is inert on any host that does not
    # include plex, exactly as it was when it lived inside this entry.
    #
    # What changed is that it needed to be its own aspect to carry a take. A
    # take is keyed by the TAKING ASPECT, so `takes.gated` written here would be
    # plex's: the vhost would move to `plex.<zone>`, the access group to
    # `svc-plex`, and the published url tautulli's own dashboard tile is derived
    # from would stop existing.

    # ── THE DASHBOARD TILE, AND WHY TAUTULLI'S IS NO LONGER HERE ──
    #
    # A `gives` is keyed by the giving ASPECT, so this one is plex's and plex's
    # only. Tautulli's tile moved to `den.aspects.tautulli` at the bottom of
    # this file, which is where it belonged all along -- that is the aspect that
    # knows it exists, carries its gate and publishes its url.
    #
    # `href` IS STATED, which is the one case the capability's null default is
    # for: plex is published as a plain vhost on `media.ivymect.in` (below)
    # rather than through the gateway, so there is no
    # `gateway.services.plex.url` for ../../services/homepage.nix to derive.
    gives =
      { self, ... }:
      {
        # ── WHAT A GRANT ON PLEX MEANS ──
        #
        # Declaring `service` is also what makes plex a HOUSEHOLD service:
        # ../../../lib/service-type.nix finds a provider by what it gives, not by
        # what it is called, so `den.households.<h>.services.plex` resolves to
        # this aspect because of this give.
        #
        # The levels are a kanidm group existing and nothing more. Plex
        # authenticates with an X-Plex-Token minted at plex.tv (see the widget
        # note below) and this vhost is a plain proxy with no gate in front, so
        # nothing checks a group on the way in -- the same inert case
        # ../../services/immich.nix, audiobookshelf and vikunja record. Sharing a
        # library is still done in Plex. What the grant buys is the household's
        # register of who uses what, which ../../services/homepage.nix reads to
        # decide whose dashboard may show it.
        service = _: {
          access = {
            levels = [
              "user"
              "admin"
            ];
            defaultLevel = "user";
          };

          # ── AND SAYING SO, RATHER THAN BEING SILENTLY INERT ──
          #
          # The paragraph above has always described this; nothing read it.
          # ../../services/access.nix's `access-grants-land` check now reports
          # every grant that realises nothing, and a grant on plex realises
          # nothing BY NATURE: the token is minted at plex.tv and sharing a
          # library happens in Plex's own UI, so there is no secret to place and
          # nothing in nix to open.
          #
          # Declared and not faked. Inventing an `X-Plex-Token` secret is the
          # wrong fix twice over -- it would need `gen-secrets`, which
          # ../../../CLAUDE.md forbids while the master key is mid-recovery, and
          # it would still not grant anybody anything.
          externallyManaged = true;
        };

        homepage-widget = _: {
          group = "Media";
          href = "https://media.ivymect.in";
          icon = "plex.svg";
          # No widget on purpose. Plex authenticates with an X-Plex-Token,
          # minted by signing in to plex.tv, so unlike the *arr keys it cannot
          # be generated -- and a widget without one does not sit quiet, it
          # 401s on every refresh and fills the journal. To enable it: add the
          # token as an agenix secret, expose it through the homepage service
          # account's env file, then set
          #   widget = { type = "plex"; url = "http://127.0.0.1:32400";
          #              key = "{{HOMEPAGE_VAR_PLEX_TOKEN}}"; };
          # -- the `url` and `key` written out, because a stated one is believed
          # and neither is derivable here.
        };
        # ── THE PORTS `openFirewall` USED TO OPEN ON EVERY WIRE ──
        #
        # nixpkgs' plex module writes `networking.firewall.allowedTCPPorts`/
        # `allowedUDPPorts`, which is the pseudo-interface `default` -- no
        # `-i`, so 32400 and four GDM ports were reachable over the tunnel and
        # the access point too. `openFirewall` is off in the `nixos` block
        # below and these are the replacement.
        tcp = _: {
          name = self.name;
          ports = {
            # THE LAN AND THE TAILNET. The lan is where the TVs and phones
            # direct-play from; the tailnet is kept because the global rule
            # this replaces already reached `tailscale0`, and narrowing it
            # away would revoke off-lan access as a refactor side effect.
            http = {
              port = sockets.http;
              meta.description = "The server: API, web UI and direct play. Dialed by TVs and phones on the lan, by nginx over loopback for media.ivymect.in, and over the tailnet from off-lan.";
              expose = [
                "lan-home"
                "tailscale"
              ];
            };
            # Companion, Roku and DLNA are all house-local control/discovery
            # paths; nothing dials them from off the lan.
            companion = {
              port = sockets.companion;
              meta.description = "Plex Companion: a phone on the lan driving playback on another client.";
              expose = [ "lan-home" ];
            };
            roku = {
              port = sockets.roku;
              meta.description = "The Roku companion channel, dialed by a Roku on the lan.";
              expose = [ "lan-home" ];
            };
            dlna = {
              port = sockets.dlna;
              meta.description = "The DLNA server's HTTP endpoint, dialed by DLNA renderers on the lan.";
              expose = [ "lan-home" ];
            };
          };
          carrier = self.name;
        };

        udp = _: {
          name = self.name;
          # GDM, the lan broadcast clients find the server with -- multicast,
          # so the lan is the only wire it means anything on.
          #
          # NO 1900 AND NO 5353 HERE, though `openFirewall` opened both:
          # ../../services/homeassistant/core.nix already declares `ssdp`
          # (1900) and `mdns` (5353) on `lan-home`, and a socket is exclusive
          # per host (../../base/capability-types.nix) -- a second giver of
          # either trips `socket-exclusions`. Both holes exist on `br0`
          # already, so nothing is lost; what is lost is the ability to say
          # plex also listens there, which is a real gap in the model for
          # multicast sockets several daemons share.
          ports = builtins.listToAttrs (
            map (port: {
              name = "gdm-${toString port}";
              value = {
                inherit port;
                meta.description = "GDM: the lan broadcast Plex clients find this server with.";
                expose = [ "lan-home" ];
              };
            }) sockets.gdm
          );
          carrier = self.name;
        };

      };

    vhosts."media.ivymect.in" = {
      # `ivymect.in`, not a per-name cert: ../secondpc/web.nix declares a
      # DNS-01 wildcard for `*.ivymect.in`, so this name is already covered and
      # the `security.acme.certs."media.pierlot.com.au"` entry that used to sit
      # below was dropped rather than renamed.
      useACMEHost = "ivymect.in";
      forceSSL = true;
      locations."/" = {
        proxyPass = "http://127.0.0.1:${toString sockets.http}";
        proxyWebsockets = true;
      };
    };
    provides.to-users.homeManager = {
      programs.aria2 = {
        enable = true;
        settings = {
          listen-port = 60000;
          dht-listen-port = 60000;
          seed-ratio = 1.0;
          max-upload-limit = "50K";
          ftp-pasv = true;
          # RPC for AriaNg + the magnet-handler page (see web.nix). Bound to
          # loopback by default (rpc-listen-all unset) -- the aria2.ivymect.in
          # vhost is the only path to it, and that vhost is basic-auth gated.
          enable-rpc = true;
          rpc-listen-port = 6800;
        };
        systemd.enable = true;
      };
    };

    nixos =
      {
        config,
        pkgs,
        scoped,
        ...
      }:
      {
        # Hardware accel (intel) — feeds Plex's LIBVA transcoding below.
        hardware.graphics = {
          enable = true;
          extraPackages = with pkgs; [
            intel-ocl
            intel-vaapi-driver
            libva-vdpau-driver
          ];
        };
        systemd.services.plex.environment.LIBVA_DRIVER_NAME = "i965";
        environment.sessionVariables.LIBVA_DRIVER_NAME = "i965";

        services.plex = {
          enable = true;
          # OFF EXPLICITLY rather than left at its default: it writes the
          # global lists, which carry no `-i`. `gives.tcp`/`gives.udp` above
          # declare the same sockets and ../../base/firewall.nix opens each on
          # the carriers it names. (avahi's same-named option defaults to
          # TRUE -- see ./samba.nix -- so the default is not to be relied on.)
          openFirewall = false;
        };

        # Shared group for the media library: plex plus whoever should browse
        # it locally get read/write onto /mnt/hdd/Media through it.
        users.groups.media = { };
        users.users.plex.extraGroups = [ "media" ];
        users.users.auscyber.extraGroups = [ "media" ];
        # Lets aria2's home-manager systemd --user service (and its RPC
        # port) run at boot without auscyber needing an active login session.
        users.users.auscyber.linger = true;

        # setgid, so every directory the *arr stack or qbittorrent creates
        # underneath inherits `media` instead of the creator's own group.
        systemd.tmpfiles.settings.media."/mnt/hdd/Media"."d" = {
          user = "plex";
          group = "media";
          mode = "2770";
        };

      };
  };

  # ── TAUTULLI, SUBORDINATE TO PLEX BY INCLUSION ──
  #
  # Watches plex's activity and history. Its own aspect only so that it can
  # carry its own `takes.gated` -- a take is keyed by the taking aspect, and
  # sharing plex's would publish it under plex's name.
  #
  # Pulled in by plex (its `includes` list at the top of this file) rather than
  # included per host, which keeps the original property: nothing to do, and
  # nothing declared, if plex is absent.

  den.aspects.tautulli = {
    includes = [ den.aspects.gateway ];

    # A PORT, NOT AN UPSTREAM: the proxy and this service are on one host by
    # construction, so the scheme and loopback address are the proxy's to supply
    # (../../services/proxy-nginx.nix). `sso` stays at its default -- tautulli
    # has no auth of its own worth relying on, so the gate is the whole of it.
    takes.gated.port = 8181;

    # Its own tile, now that the tile is a capability keyed by the aspect that
    # gives it. No `href`: the gate publishes this at
    # `gateway.services.tautulli.url` and ../../services/homepage.nix reads it.
    #
    # No widget: tautulli generates its own API key on first run with no way to
    # set it, so a widget here would 401 on every refresh. A link until a key is
    # pasted in -- and it is not the gateway's key, so admitting homepage as an
    # api client would not help.
    gives = _: {
      homepage-widget = _: {
        group = "Media";
        icon = "tautulli.svg";
      };
    };

    nixos = {
      # Loopback only: nginx is the way in and oauth2-proxy is in front of it.
      # `media` gets it read access to the library so its per-file stats line up
      # with what plex sees.
      services.tautulli = {
        enable = true;
        port = 8181;
        group = "media";
      };
    };
  };
}
