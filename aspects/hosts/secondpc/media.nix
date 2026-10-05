{ den, ... }:
# secondpc media stack + service secrets, ported from the old dotfiles
# navidrome.nix / general/default.nix. Uses raw age.secrets/age.templates in the
# nixos block (faithful to the originals; the agenix-rekey `secrets` class routes
# to the same option). Generated/rekeyed secrets all need `nix run .#rekey` and
# `nix run .#gen-secrets` before deploy.
{
  den.aspects.secondpc = {
    includes = [
      # ── TWO SERVICES OUT OF THIS HOST CATCH-ALL ──
      #
      # navidrome and seerr ran inside `den.aspects.secondpc.nixos` below while
      # their vhosts sat in ./web.nix -- one service across two aspects, neither
      # of them about that service. Fine for class content, which den merges;
      # not fine for a take, which is keyed by the taking ASPECT, so a
      # `takes.proxy` written here would be the HOST's and the second would
      # collide with the first.
      #
      # Declared at the bottom of this file, included here rather than per host,
      # so what secondpc runs still reads off this one list.
      den.aspects.navidrome
      den.aspects.seerr

      den.aspects.agenix-rekey
      den.aspects.user-pwd
      den.aspects.gateway
      den.aspects.sonarr
      den.aspects.radarr
      den.aspects.lidarr
      den.aspects.bazarr
      den.aspects.qbittorrent
    ];

    nixos =
      {
        config,
        pkgs,
        lib,
        scoped,
        ...
      }:
      {
        # Media + downloading
        services.audiobookshelf.enable = true;
        # uid/gid pinned because the soularr container (slskd.nix) can only
        # name the account numerically. On a host that already allocated `media`
        # dynamically this needs a one-off `chown -R media:media /mnt/hdd`.
        # 1000, not a fresh number: the group predates this pin (plex.nix created
        # it) and NixOS will not renumber an existing group -- it silently keeps
        # the old gid while Nix believes the new one. That divergence is invisible
        # until something reads the gid at eval time, as slskd.nix does to build
        # the soularr container's numeric user, which then ran as a gid that does
        # not exist. Aligning to reality also means no data has to be chowned.
        users.groups.media.gid = 1000;
        users.users.media = {
          isSystemUser = true;
          group = "media";
          uid = 700;
        };
        # navidrome is its own aspect at the bottom of this file -- it needs one
        # to carry its own `takes.proxy`, since a take is keyed by the taking
        # aspect and this one is the host's catch-all.
        # sonarr / radarr / lidarr / bazarr / qbittorrent each live in their own
        # aspect under ../../services/media, where their gateway entry and auth
        # config sit next to the service itself. prowlarr is in
        # ../../services/prowlarr.nix, with the flaresolverr it drives.

        # Request frontend (browse/request shows+movies, forwards to
        # sonarr/radarr) -- nixpkgs merged jellyseerr/overseerr into one
        # package, still supports Plex as its backend via its own setup
        # wizard. Tautulli moved to plex.nix, which is whose stats they are.
        # seerr is its own aspect at the bottom of this file, for the same
        # reason as navidrome.

        # NO `gateway.serviceAccounts` HERE ANY MORE. `seerr` and `homepage`
        # were declared by hand because "nothing registers them automatically",
        # which was true and was the bug: the callee named the caller in
        # `api.clients` and the caller was named again here, with nothing
        # checking the two agreed. ../../services/gateway.nix now derives the
        # account set from `api.clients`, and each caller states how it reads
        # its own key through `takes.credential` -- homepage in
        # ../../services/homepage.nix, seerr below.

        # --- user password: ivy-password (source, intermediary) hashed into
        #     ivy-pwd-hash (generated via openssl passwd -6). ---

        # --- vaultwarden admin token: regenerated as an agenix secret (the
        #     original used sops). Fresh random token on gen-secrets. ---
        age.secrets."vaultwarden.env".generator.script =
          {
            pkgs,
            lib,
            ...
          }:
          ''
            printf 'ADMIN_TOKEN=%s\n' "$(${lib.getExe pkgs.openssl} rand -base64 48)"
          '';
        services.vaultwarden.environmentFile = config.age.secrets."vaultwarden.env".path;

        systemd.tmpfiles.settings.media = {
          "/mnt/hdd/AudioBooks"."d" = {
            user = "audiobookshelf";
            group = "media";
            mode = "0770";
          };
          "/mnt/hdd/Music/Downloads"."d" = {
            user = "media";
            group = "media";
            mode = "0770";
          };
        };

        # --- loki logs behind nginx basic-auth. htpasswd is regenerated as an
        #     agenix secret (original was sops) from ivy-password for user "ivy". ---
        age.secrets.htpasswd = {
          owner = config.services.nginx.user;
          generator = {
            dependencies = { inherit (scoped.user-pwd.secrets) ivy-password; };
            script =
              {
                pkgs,
                lib,
                decrypt,
                deps,
                ...
              }:
              ''
                ${pkgs.apacheHttpd}/bin/htpasswd -nbB ivy "$(${decrypt} ${lib.escapeShellArg deps.ivy-password.file})"
              '';
          };
        };
        security.acme.certs."logs.pierlot.com.au" = {
          environmentFile = scoped.cloudflare.secrets.acme_env.path;
          group = config.services.nginx.group;
        };
        services.nginx.virtualHosts."logs.pierlot.com.au" = {
          useACMEHost = "logs.pierlot.com.au";
          forceSSL = true;
          basicAuthFile = config.age.secrets.htpasswd.path;
          locations."/" = {
            proxyPass = "http://localhost:3100";
            recommendedProxySettings = true;
          };
        };
      };
  };

  den.aspects.navidrome = {
    includes = [ den.aspects.gateway ];

    # WAS A RAW VHOST in ./web.nix: `music.ivymect.in` -> 127.0.0.1:4533, with
    # `useACMEHost` and `forceSSL` written out. `takes.proxy` says only what is
    # this service's own -- the name and the port -- and
    # ../../services/proxy-nginx.nix supplies the rest. The ACME host it uses
    # is `gateway.acmeHost`, which defaults to the same zone, so the rendered
    # vhost is the same.
    #
    # `proxy`, NOT `gated`: this was never behind oauth2-proxy and converting
    # it to `takes.gated` would have put it there.
    takes.proxy = {
      subdomain = "music";
      port = 4533;
    };

    # ── A TILE FROM A `proxy` TAKE RATHER THAN A `gated` ONE ──
    #
    # Worth having as the case that proves the derivation: this service is
    # published through `takes.proxy` and not `takes.gated`, and
    # ../../services/proxy-nginx.nix still writes a `gateway.services.navidrome`
    # entry for it -- so `url` exists and ../../services/homepage.nix derives
    # the href from it with nothing stated here.
    #
    # No widget: homepage's navidrome widget wants a user, a salt and a token,
    # none of which is the gateway credential, so this is a link.
    gives = _: {
      homepage-widget = _: {
        group = "Media";
        icon = "navidrome.svg";
        description = "Music";
      };
    };

    # Generated in this aspect's own scope. Last.fm and Spotify keys are the
    # providers' to issue, so those integrations stay off until one is added.
    # A new key means users' stored passwords must be set again.
    secrets.password-encryption-key = {
      generator.script = "random-base64-32";
      restartUnits = [ "navidrome.service" ];
    };
    templates.env =
      { secrets, ... }:
      {
        restartUnits = [ "navidrome.service" ];
        dependencies = { inherit (secrets) password-encryption-key; };
        content =
          { placeholders, ... }:
          ''
            ND_PASSWORDENCRYPTIONKEY=${placeholders.password-encryption-key}
          '';
      };

    nixos =
      { scoped, ... }:
      {
        services.navidrome = {
          enable = true;
          group = "media";
          environmentFile = scoped.navidrome.templates.env.path;
          settings = {
            user = "media";
            MusicFolder = "/mnt/hdd/Music";
          };
        };
      };
  };

  # Request frontend (browse/request shows+movies, forwards to sonarr/radarr) --
  # nixpkgs merged jellyseerr/overseerr into one package, still supports Plex as
  # its backend via its own setup wizard. Tautulli moved to ./plex.nix, which is
  # whose stats they are.
  den.aspects.seerr = {
    includes = [ den.aspects.gateway ];

    takes.proxy = {
      subdomain = "requests";
      port = 5055;
    };

    # A CALLER AND NOT A GATED API. seerr forwards requests to sonarr and
    # radarr, which name it in their own `api.clients`
    # (../../services/media/servarr.nix) -- so that is what mints its account,
    # and all this end owes is what the account is FOR. It reads its key from
    # its own configuration rather than the environment, so no `envPrefix` and
    # nothing to restart.
    takes.credential.description = "Request frontend, talks to sonarr and radarr";

    # The dashboard tile. No widget: homepage's seerr widget authenticates with
    # seerr's OWN api key, which seerr generates and nothing here holds -- and
    # the credential above is the key seerr PRESENTS to sonarr and radarr, which
    # is the other direction entirely.
    gives = _: {
      homepage-widget = _: {
        group = "Media";
        icon = "jellyseerr.svg";
        description = "Requests";
      };
    };

    nixos.services.seerr.enable = true;
  };
}
