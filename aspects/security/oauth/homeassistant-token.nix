{ den, lib, ... }:
# A Home Assistant long-lived access token, minted declaratively.
#
# ============================================================================
# THIS IS INERT. Nothing includes `den.aspects.homeassistant-token`, so no host
# evaluates it, no secret is declared on any node, and `agenix generate` cannot
# see it. Landing this file changes no host's evaluated config.
#
# TO TURN IT ON: add `den.aspects.homeassistant-token` to the `includes` of the
# scope that should hold the token -- the person's user scope, since the subject
# of an HA long-lived token is an HA USER, not a machine.
#
# BEFORE THAT, two things must be true, and neither is checkable from here:
#
#   1. The client is registered. HA is IndieAuth-shaped, so there is no app
#      registration in the usual sense -- but the `client_id` below is a URL and
#      HA may try to FETCH it. See the `haClient` comment in ./_lib.nix; this is
#      the unverified part and the first thing to suspect if the authorization
#      request is rejected.
#
#   2. HA *and* kanidm are both reachable from the admin host at `agenix
#      generate` time. HA is the authorization server and kanidm is the identity
#      provider behind it (`auth_oidc`), so the browser leg is HA -> kanidm ->
#      back and BOTH must be up. This is a stronger precondition than the
#      Google/Microsoft generators, which only ever talk to public endpoints.
#
# AND READ THIS FIRST: re-minting does not replace the old token. HA keeps a
# server-side refresh-token record per long-lived token and issuing a new one
# does not revoke the previous one, which stays valid for the remainder of its
# ten years. So every `derivedFrom` change here LEAVES A LIVE CREDENTIAL BEHIND
# that must be revoked by hand (POST /auth/revoke, or the user's profile page).
# Editing `lifespan` or `clientName` below is "mint another credential and go
# revoke the old one", not a rotation. ./_lib.nix `haLongLivedToken` has the
# detail.
# ============================================================================
let
  oauth = import ./_lib.nix { inherit lib; };

  # Where Home Assistant is. The token is minted against this instance and is
  # only valid there.
  url = "https://ha.example.invalid";

  # Whose token this is. An HA long-lived token belongs to an HA user, so the
  # subject is a person; ./_lib.nix keys the file by this name so every host
  # that person has an account on names one file.
  sub = "ivy";

  # What HA shows in the user's profile next to the token, and how long it
  # lives. Both are in `derivedFrom`, so changing either re-mints -- which,
  # given the paragraph above, also means orphaning the previous one.
  clientName = "agenix-rekey";
  lifespan = 3650;

  # IndieAuth couples `client_id` and the redirect URI to the same host and
  # port, so one port decides both. Deliberately not the port the Google and
  # Microsoft generators use: two generators must never want the same loopback
  # listener.
  client = oauth.haClient 49153;
in
{
  den.aspects.homeassistant-token = {
    includes = [ den.aspects.agenix-rekey ];

    secrets.token = oauth.tokenSecret {
      provider = oauth.homeassistant url;
      service = "homeassistant";
      inherit sub;
      inherit (client) clientId port;

      # Tier 1 vanishes here. HA's authorization-code flow has no client
      # secret, so this is structurally the public-client shape and there is
      # nothing confidential to keep on the admin host in the first place --
      # `tokenSecret` passes `--auth-method none` on the strength of this null.
      clientSecret = null;

      # HA's /auth/authorize takes no scopes; the token's authority is the
      # user's.
      scopes = [ ];

      # The OAuth token is only a session. This spends it on the real secret
      # over HA's websocket API, which is the sole way to mint one.
      postProcess = oauth.haLongLivedToken { inherit url clientName lifespan; };
      extraDerivedFrom = { inherit clientName lifespan; };
    };
  };
}
