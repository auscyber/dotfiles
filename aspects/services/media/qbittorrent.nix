{ den, accessLib, ... }:
let
  port = 9090;
in
{
  den.aspects.qbittorrent = {
    # The dashboard tile is a capability this aspect gives
    # (`gives.homepage-widget` below), so it no longer includes the dashboard.
    includes = [ den.aspects.gateway ];

    # ── WHAT IT LISTENS ON, so the proxy can ASK rather than assume ──
    #
    # The port used to be a number inside `takes.gated`, which meant the only
    # thing that could reach this service was the gate, and only because it
    # hardcoded `http://127.0.0.1:<that number>`. Given here it is an endpoint:
    # ../../base/capability.nix composes it with whichever carrier the two ends
    # share -- the loopback for nginx on this box -- and the proxy resolves the
    # upstream instead of assuming one.
    #
    # NOT EXPOSED: loopback only, because the gate is the way in. The firewall
    # is derived from this (../../base/firewall.nix), so saying nothing here
    # means the port stays shut on every wire.
    gives =
      { self, ... }:
      {
        # WHAT THE HOUSE OFFERS: tagged `arr`, even though it is published on its own subdomain -- the tag is about ACCESS, not publishing.
        service = _: {
          group = "arr";
        };

        # A LINK AND NO WIDGET. qbittorrent's widget wants a username and a
        # password, and LocalHostAuth (below) means there is no longer an
        # account to give it -- so there is no declarative API-key surface here
        # at all, and `keyField` is moot: an empty `widget` is a tile homepage
        # draws as a plain link.
        homepage-widget = _: {
          group = "Downloads";
          icon = "qbittorrent.svg";
        };

        tcp = _: {
          name = self.name;
          ports.http = {
            inherit port;
            meta.description = "qbittorrent's WebUI and WebAPI, which asks a loopback caller for no login at all -- so nginx has to be the only path to it.";
          };
        };
      };

    # ── WHAT A GRANT ON THE `arr` TAG DOES ──
    #
    # Keyed on the TAG, not on this app: one grant on the household's `arr`
    # group covers all six of these apps, and the gate is how it is honoured.
    # ../access.nix used to write it centrally for every service in the tree --
    # the gate is these apps' own mechanism, so it is declared beside the take
    # that creates it.
    gated.qbittorrent.extraGroups = accessLib.groupsFor "arr";

    # Out through a private route (../../network/routes.nix), and not at all
    # when it is down.
    takes.route = {
      segment = "internet";
      private = true;
    };

    takes.gated = {
      # WHO MAY CALL IT -- the service's own fact, which any gate can enforce.
      # The HOW (which header, which prefix, whether a key is swapped) is the
      # proxy's and lives on `takes.nginx` below.
      clients = [
        # Not "prowlarr": prowlarr has no download client of its own
        # (downloadClient = false in prowlarr.nix) and never calls
        # qbittorrent, so granting it a key here only fed a false positive
        # into prowlarr-connect's target selection -- which reads "prowlarr
        # holds a key for X" as "X belongs in prowlarr's Applications list"
        # and then 400s because qbittorrent has no Applications schema
        # entry to sync (it's a download client, not an indexer-sync app).
        "sonarr"
        "radarr"
        "lidarr"
      ];

      # qBittorrent 5.1 (WebAPI 2.14.1) takes a key as `Authorization: Bearer`,
      # so it joins the per-caller scheme like the *arr apps do.
      #
      # `internalKey = false` on purpose: the only declarative way to give
      # qbittorrent a key of its own is `WebUI\APIKey` in serverConfig, and
      # serverConfig is a world-readable store file. So the gateway validates
      # the caller's key and then STRIPS it, and qbittorrent is left trusting
      # loopback via LocalHostAuth below -- nginx is the only path to it, so the
      # gate is the same, without a secret in the store.
      # qBittorrent.conf is installed read-only (see serverConfig below), so the
      # settings dialog silently discards everything typed into it. Say so in
      # the UI rather than letting it look like it worked.
      banner = "Read-only - qBittorrent's settings are declared in Nix; changes made here are discarded";
    };

    # ── NGINX-SPECIFIC, SO DECLARED AS SUCH ──
    #
    # The key-rewrite machinery is a fact about the PROXY, not about this
    # service: which header carries the key, whether it is swapped for an
    # internal one, which env var the service reads it from. Taking `nginx`
    # says this service depends on that particular proxy -- a host running
    # traefik does not give `nginx`, so this fails to resolve rather than
    # publishing with the rewrite silently missing.
    #
    # `clients` stays on `takes.gated`: who may call this is the service's own
    # fact and any gate can enforce it.
    takes.nginx = {
      api = {
        enable = true;
        header = "Authorization";
        headerPrefix = "Bearer ";
        internalKey = false;
      };
    };

    nixos = { config, ... }: {
      services.qbittorrent = {
        enable = true;
        # `media` as the PRIMARY group, not a supplementary one, so everything
        # it writes is group-owned by the library's group. With UMask 0002 and
        # the setgid bit on the library root, what it downloads is directly
        # writable by whichever *arr imports it.
        group = "media";
        webuiPort = port;

        # NOTE: setting serverConfig at all makes the module install
        # qBittorrent.conf as a read-only symlink, so the WebUI can no longer
        # persist ANY setting -- everything worth keeping has to be declared
        # here from now on.
        serverConfig = {
          LegalNotice.Accepted = true;
          Preferences = {
            WebUI = {
              # nginx reaches it over loopback and oauth2-proxy has already
              # established who the user is, so a second login here would be
              # asking the same question twice. This is qbittorrent's
              # equivalent of the servarr apps' `auth.method = "External"`.
              LocalHostAuth = false;
              # The *arr apps connect from the host itself; the LAN and the
              # wireguard subnet are the same trust boundary samba.nix uses.
              AuthSubnetWhitelistEnabled = true;
              AuthSubnetWhitelist = "127.0.0.1/32, 192.168.0.0/24, 10.100.0.0/24";
              # Both of these reject proxied requests: the Host header is the
              # public name, not the loopback address it listens on.
              CSRFProtection = false;
              HostHeaderValidation = false;
              Address = "127.0.0.1";
            };
            Downloads.SavePath = "/mnt/hdd/Media/downloads";
          };
        };
      };
      systemd.services.qbittorrent.serviceConfig.UMask = "0002";
    };
  };
}
