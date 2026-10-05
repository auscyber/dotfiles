{
  den,
  lib,
  ...
}:
let
  oauth = import ../security/oauth/_lib.nix { inherit lib; };

  # Whose tokens these are. An OAuth refresh token's subject is a PERSON, not a
  # host: the same token is the right token on every machine ivy has an account
  # on, so it is minted once and ../security/oauth/_lib.nix keys its file by
  # this name. Every host's declaration then names one file, which agenix-rekey
  # dedups because the generator is identical.
  sub = "ivy";

  # `client_id` is not a secret -- it travels in the authorization URL the
  # operator's browser visits -- so it lives here in the clear, which is also
  # what lets `generator.derivedFrom` track its value rather than merely which
  # secret holds it. REPLACE THESE with the ids from the respective app
  # registrations; the redirect URI to register is
  # `http://127.0.0.1:49152/callback` for both.
  clientIds = {
    gdrive = "CHANGEME.apps.googleusercontent.com";
    onedrive-personal = "CHANGEME-0000-0000-0000-000000000000";
  };

  remotes = {
    # Google. A confidential client: Google's token endpoint wants the client
    # secret on the code exchange, so the secret is a generator DEPENDENCY --
    # decrypted with the master identity while `agenix generate` runs and
    # declared `intermediary`, so agenix-rekey never encrypts it to a host key.
    # Only the resulting refresh token is rekeyed outward.
    #
    # The cost of that is real and is not papered over here: rclone renews an
    # expired access token itself, and it needs the client secret to do it. With
    # the secret admin-only, this remote works until the first access token
    # expires. Deploying the secret to fix that is the one thing this design
    # will not do on its own -- see the report; a native-client registration
    # (below) is the way out, not a second copy of a confidential secret.
    gdrive = {
      provider = oauth.providers.google;
      scopes = [ "https://www.googleapis.com/auth/drive" ];
      confidential = true;
      config = {
        type = "drive";
        scope = "drive";
      };
    };

    # Microsoft. Registered as a PUBLIC client ("Mobile and desktop
    # applications"), which has no client secret at all: PKCE is what binds the
    # code to this client, so `--auth-method none` is correct rather than a
    # weakening. Nothing confidential exists to keep off the host, and rclone
    # can renew an access token with `client_id` plus the refresh token -- so
    # this is the shape where "the token is the only thing deployed" is true
    # end to end.
    onedrive-personal = {
      provider = oauth.providers.microsoft;
      scopes = [ "Files.ReadWrite.All" ];
      confidential = false;
      config = {
        type = "onedrive";
        drive_id = "7BEE81A1BAA61272";
        drive_type = "personal";
      };
    };
  };

  confidentialRemotes = lib.filterAttrs (_: r: r.confidential) remotes;
in
{
  den.aspects.rclone = {
    includes = [ den.aspects.agenix-rekey ];

    # Nested under this aspect's own scope by aspects/security/age-scope.nix, so
    # each entry lands at `age.secrets."rclone/<name>"` -- the key here is just
    # the name, no hand-written prefix.
    secrets =
      # Tier 1, for the confidential clients only: the operator-entered client
      # secret. `agenix edit secrets/static/oauth/<provider>-rclone-client-secret.age`
      # creates it. Never read on a host, so never rekeyed to one.
      lib.mapAttrs' (
        name: r:
        lib.nameValuePair "${name}-client-secret" (
          oauth.clientSecret {
            provider = r.provider.name;
            service = "rclone";
          }
        )
      ) confidentialRemotes

      # Tier 2: the refresh token itself, minted interactively.
      // lib.mapAttrs (
        name: r:
        oauth.tokenSecret {
          inherit (r) provider scopes;
          inherit sub;
          service = "rclone";
          clientId = clientIds.${name};
          clientSecret =
            if r.confidential then
              oauth.clientSecret {
                provider = r.provider.name;
                service = "rclone";
              }
            else
              null;
        }
      ) remotes;

    homeManager =
      {
        config,
        scoped,
        ...
      }:
      {
        programs.rclone = {
          enable = true;
          remotes = lib.mapAttrs (name: r: {
            config = r.config // {
              # Public half of the credentials. rclone needs it to renew an
              # access token, and it is the same id the token was minted
              # against -- a refresh token is bound to its client.
              client_id = clientIds.${name};
            };
            # The whole point: the token path is read off the scope, never
            # written by hand. The file holds exactly what rclone expects in its
            # `token` key -- a json.Marshal of golang.org/x/oauth2.Token.
            secrets.token = scoped.rclone.secrets.${name}.path;
            mounts."" = {
              enable = true;
              mountPoint = "${config.home.homeDirectory}/mnts/${name}";
            };
          }) remotes;
        };
      };
  };
}
