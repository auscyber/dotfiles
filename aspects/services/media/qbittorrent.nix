{ den, ... }:
let
  port = 9090;
in
{
  den.aspects.qbittorrent = {
    includes = [
      den.aspects.gateway
      den.aspects.homepage
    ];

    # No declarative API-key surface -- qbittorrent authenticates callers with
    # a username and password, not a key -- so the browser gate is the whole
    # story at the edge, and the bypasses below are what keep the *arr apps and
    # nginx from needing a credential at all.
    homepage = { config, ... }: {
      qbittorrent = {
        group = "Downloads";
        href = config.gateway.services.qbittorrent.url;
        icon = "qbittorrent.svg";
        # No widget: qbittorrent's widget wants a username and password,
        # and LocalHostAuth means there is no longer an account to give it.
      };
    };

    gated.qbittorrent = {
      upstream = "http://127.0.0.1:${toString port}";
      # qBittorrent 5.1 (WebAPI 2.14.1) takes a key as `Authorization: Bearer`,
      # so it joins the per-caller scheme like the *arr apps do.
      #
      # `internalKey = false` on purpose: the only declarative way to give
      # qbittorrent a key of its own is `WebUI\APIKey` in serverConfig, and
      # serverConfig is a world-readable store file. So the gateway validates
      # the caller's key and then STRIPS it, and qbittorrent is left trusting
      # loopback via LocalHostAuth below -- nginx is the only path to it, so the
      # gate is the same, without a secret in the store.
      api = {
        enable = true;
        header = "Authorization";
        headerPrefix = "Bearer ";
        internalKey = false;
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
      };
      # qBittorrent.conf is installed read-only (see serverConfig below), so the
      # settings dialog silently discards everything typed into it. Say so in
      # the UI rather than letting it look like it worked.
      banner = "Read-only - qBittorrent's settings are declared in Nix; changes made here are discarded";
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
