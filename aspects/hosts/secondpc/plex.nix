{ den, ... }:
# Plex, standing in for jellyfin on secondpc: same Intel VAAPI render-device
# passthrough and the same /mnt/hdd media library jellyfin had access to,
# fronted at media.pierlot.com.au instead of jellyfin.pierlot.com.au.
# `jellyfin.nix` is kept around (unincluded) in case we ever switch back.
{
  den.aspects.plex = {
    includes = [ den.aspects.secondpc-web ];

    vhosts."media.pierlot.com.au" = {
      useACMEHost = "media.pierlot.com.au";
      forceSSL = true;
      locations."/" = {
        proxyPass = "http://127.0.0.1:32400";
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
          openFirewall = true;
        };

        # Shared group for the media library: plex plus whoever should browse
        # it locally get read/write onto /mnt/hdd/Media through it.
        users.groups.media = { };
        users.users.plex.extraGroups = [ "media" ];
        users.users.auscyber.extraGroups = [ "media" ];
        # Lets aria2's home-manager systemd --user service (and its RPC
        # port) run at boot without auscyber needing an active login session.
        users.users.auscyber.linger = true;

        systemd.tmpfiles.settings.media."/mnt/hdd/Media"."d" = {
          user = "plex";
          group = "media";
          mode = "0770";
        };

        security.acme.certs."media.pierlot.com.au" = {
          environmentFile = scoped.secondpc-web.secrets."acme_cloudflare.env".path;
          group = config.services.nginx.group;
        };
      };
  };
}
