{ config, pkgs, ... }:
{
  services.qbittorrent = {
    enable = true;
    webuiPort = 9090;
    openFirewall = true;
  };

  nixpkgs.config.packageOverrides = pkgs: {
    intel-vaapi-driver = pkgs.intel-vaapi-driver.override { enableHybridCodec = true; };
  };
  systemd.services.jellyfin.environment.LIBVA_DRIVER_NAME = "i965"; # or i965 for older GPUs
  environment.sessionVariables = {
    LIBVA_DRIVER_NAME = "i965";
  };

  security.acme = {
    acceptTerms = true;
    defaults.email = "ivyp@outlook.com.au";
    certs."jellyfin.pierlot.com.au" = {
      dnsProvider = "cloudflare";
      environmentFile = config.age.secrets."acme_cloudflare.env".path;
      group = config.services.nginx.group;
    };
    certs."home.pierlot.com.au" = {
      dnsProvider = "cloudflare";
      environmentFile = config.age.secrets."acme_cloudflare.env".path;

      group = config.services.nginx.group;
    };
    certs."grafana.pierlot.com.au" = {
      dnsProvider = "cloudflare";
      environmentFile = config.age.secrets."acme_cloudflare.env".path;

      group = config.services.nginx.group;
    };
    certs."nextcloud.pierlot.com.au" = {
      dnsProvider = "cloudflare";

      environmentFile = config.age.secrets."acme_cloudflare.env".path;

      group = config.services.nginx.group;

    };
  };

  hardware.graphics = {
    enable = true;

    extraPackages = with pkgs; [
      intel-ocl # Generic OpenCL support

      # For Broadwell and newer (ca. 2014+), use with LIBVA_DRIVER_NAME=iHD:

      # For older processors, use with LIBVA_DRIVER_NAME=i965:
      intel-vaapi-driver
      libva-vdpau-driver

      # For 13th gen and newer:

      # For older processors:

      # For 11th gen and newer:

      # Deprecated (may not build on recent channels):
      # intel-media-sdk
    ];
  };
  networking.firewall.allowedTCPPorts = [ 80 ];
  services.nginx = {
    enable = true;
    recommendedProxySettings = true;
    recommendedTlsSettings = true;
    recommendedOptimisation = true;
    virtualHosts = {
      "nextcloud.pierlot.com.au" = {
        useACMEHost = "nextcloud.pierlot.com.au";
        forceSSL = true;
        http2 = true;
        locations."/" = {
          proxyPass = "http://127.0.0.1:11000";
          proxyWebsockets = true;
        };

      };
      "home.pierlot.com.au" = {
        useACMEHost = "home.pierlot.com.au";
        forceSSL = true;
        http2 = true;
        locations."/" = {
          proxyPass = "http://192.168.0.37:8123";
          proxyWebsockets = true;
        };
      };
      "jellyfin.pierlot.com.au" = {
        useACMEHost = "jellyfin.pierlot.com.au";

        forceSSL = true;
        http2 = true;
        locations."/" = {
          proxyPass = "http://127.0.0.1:8096";
          proxyWebsockets = true;
        };
        extraConfig = ''
          		client_max_body_size 20M;
                    		add_header X-Content-Type-Options "nosniff";
                    		 add_header Permissions-Policy "accelerometer=(), ambient-light-sensor=(), battery=(), bluetooth=(), camera=(), clipboard-read=(), display-capture=(), document-domain=(), encrypted-media=(), gamepad=(), geolocation=(), gyroscope=(), hid=(), idle-detection=(), interest-cohort=(), keyboard-map=(), local-fonts=(), magnetometer=(), microphone=(), payment=(), publickey-credentials-get=(), serial=(), sync-xhr=(), usb=(), xr-spatial-tracking=()" always;

                    		'';

      };

    };
  };
  services.tailscale = {
    enable = true;
    authKeyFile = config.sops.secrets."tailscale/auth_key".path;
    extraSetFlags = [ "--advertise-exit-node" ];
  };

  services.jellyfin = {
    enable = true;
    openFirewall = true;
  };
  sops.secrets."tailscale/auth_key" = {
    sopsFile = ../../../secrets/secondpc/default.yaml;
  };
  age.secrets."acme_cloudflare.env".rekeyFile = ./acme_cloudflare.age;

}
