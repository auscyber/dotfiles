{ den, ... }:
# kanidm as a login provider for Home Assistant: the OAuth2 client on the IdP
# side, and the `auth_oidc` custom component on HA's side.
{
  den.aspects.homeassistant = {
    # The client name must be `home-assistant` while this aspect is
    # `homeassistant` -- the name is in the OIDC issuer URL that hass itself is
    # configured with. The shared OIDC shape is defaulted by
    # `gateway.oidcClients` in ../gateway.nix.
    kanidmClients.home-assistant = {
      displayName = "Home Assistant";
      # Published at `home.<domain>`, not `home-assistant.<domain>`.
      subdomain = "home";
      callback = "/auth/oidc/callback";
      # A PUBLIC client: the HA frontend does PKCE in the browser and has
      # nowhere to keep a secret, hence no `secretFile`.
      public = true;
      # `auth_oidc.roles` below matches on bare group names, so they have to
      # arrive in the token.
      groupsName = true;
    };

    nixos =
      { config, pkgs, ... }:
      let
        # Derived from this host's `gateway.domain` rather than written out, so
        # the aspect carries no literal zone -- the same reason
        # `gateway.oidcClients.<n>.domain` defaults to it (../gateway.nix).
        #
        # The `auth` subdomain IS still a literal: kanidm's own origin is
        # `services.kanidm.server.settings.origin` in
        # ../../hosts/secondpc/sso.nix and there is no host-level option
        # carrying it, and reading that option from here would make this aspect
        # fail to evaluate on any host that does not also run kanidm.
        issuer = "https://auth.${config.gateway.domain}/oauth2/openid/home-assistant";
      in
      {
        # The OIDC client itself is declared above, through the `kanidmClients`
        # class. It is a PUBLIC client (PKCE, no secret): hass-oidc-auth defaults
        # to public and kanidm enforces PKCE for one, so there is no client
        # secret to generate, rekey, or get into a store-rendered
        # configuration.yaml.

        # auth_oidc is the component HACS used to install into the config dir
        # (`custom_components/auth_oidc`, v1.2.1 there), declared rather than
        # fetched at runtime.
        #
        # NOT the same release. This comment used to claim nixpkgs packaged
        # v1.2.1 as well; on the nixpkgs this flake locks it is
        # hass-oidc-auth **v0.6.3-alpha**
        # (pkgs/servers/home-assistant/custom-components/auth_oidc/package.nix).
        # Every key set below was checked against THAT version's
        # `custom_components/auth_oidc/config.py` CONFIG_SCHEMA and is valid
        # there -- `client_id`, `discovery_url`, `display_name`,
        # `groups_scope`, `features.{automatic_user_linking,
        # automatic_person_creation, force_https}` and `roles.{admin,user}`.
        # The schema is `extra = vol.REMOVE_EXTRA`, so a key that version does
        # not know is silently DROPPED rather than reported -- which is why
        # this has to be checked rather than assumed when the pin moves.
        services.home-assistant.customComponents = with pkgs.home-assistant-custom-components; [
          auth_oidc
        ];

        # kanidm as an additional login provider alongside HA's own, which
        # stays enabled so there is still a way in if the IdP is down.
        services.home-assistant.config.auth_oidc = {
          client_id = "home-assistant";
          # THE PUBLIC URL, and it has to be: the discovery document's
          # `authorization_endpoint` is where the user's BROWSER is sent, so a
          # loopback fetch would have to be to a kanidm whose own origin is
          # still the public name -- and kanidm serves TLS itself with the
          # wildcard certificate, which does not match 127.0.0.1, so the fetch
          # would fail certificate verification instead.
          #
          # The cost is the hazard ../nextcloud.nix and
          # ../../hosts/secondpc/sso.nix:385 both document: a service on this
          # box reaching another one on this box by its public name puts DNS,
          # TLS and the cloudflare tunnel on the path. Unlike oauth2-proxy,
          # auth_oidc does not fetch this at startup -- it is read on the first
          # login attempt -- so HA still comes up with the tunnel down and only
          # the kanidm login button stops working. That is why there is no
          # `after = [ "nginx.service" "kanidm.service" ]` here the way sso.nix
          # needed one.
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
            # Creates the HA person for a kanidm login, which is why
            # ./core.nix's `config` has no `person:` block -- see the note
            # there. Already the component's default (`features.get(...,
            # True)` in v0.6.3-alpha's provider.py), unlike
            # `automatic_user_linking` above which defaults to False and so is
            # the one of the two that is load bearing. Stated anyway: it is
            # what makes the absent `person:` block correct, and a default is
            # not a promise.
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
}
