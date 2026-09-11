{ den, ... }:
# Jellyfin, split out of the secondpc media stack into a standalone aspect.
# Nothing includes this — add `den.aspects.jellyfin` to a host's includes to
# run it. Owns its own Intel VAAPI transcode passthrough, the jellyfin
# service/packages, and the jellyfin.pierlot.com.au vhost + ACME cert (DNS-01
# creds come from `den.aspects.secondpc-web`).
{
  den.aspects.jellyfin = {
    includes = [ den.aspects.secondpc-web ];

    vhosts."jellyfin.pierlot.com.au" = {
      useACMEHost = "jellyfin.pierlot.com.au";
      forceSSL = true;
      locations."/" = {
        proxyPass = "http://127.0.0.1:8096";
        proxyWebsockets = true;
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
        # Hardware accel (intel) — feeds jellyfin's LIBVA transcoding below.
        hardware.graphics = {
          enable = true;
          extraPackages = with pkgs; [
            intel-ocl
            intel-vaapi-driver
            libva-vdpau-driver
          ];
        };
        systemd.services.jellyfin.environment.LIBVA_DRIVER_NAME = "i965";
        environment.sessionVariables.LIBVA_DRIVER_NAME = "i965";

        services.jellyfin = {
          enable = true;
          openFirewall = true;
        };

        environment.systemPackages = with pkgs; [
          jellyfin
          jellyfin-web
          jellyfin-ffmpeg
        ];

        security.acme.certs."jellyfin.pierlot.com.au" = {
          environmentFile = scoped.secondpc-web.secrets."acme_cloudflare.env".path;
          group = config.services.nginx.group;
        };
      };
  };
}
