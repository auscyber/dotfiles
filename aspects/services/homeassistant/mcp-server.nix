{ den, ... }:
# Home Assistant AS an MCP server: the `mcp_server` integration, so a model
# client can list and call the entities HA exposes to Assist.
#
# ===========================================================================
# NOT TURNED ON. This aspect is declared and referenced by NOTHING -- no host
# includes it and `den.aspects.homeassistant` does not either, so secondpc's
# evaluated configuration is byte-identical with this file present or absent.
# To enable it, add ONE line to ../../hosts/secondpc/default.nix's `includes`:
#
#     den.aspects.homeassistant-mcp
#
# and then do the two things that line cannot do -- see "what else must happen
# first" below. Until both are done, enabling this adds a python closure and an
# unreachable HTTP endpoint and nothing else.
# ===========================================================================
#
# --- what was verified, and against what ---
#
# Everything below is from the integration's own source at the HA release this
# flake's nixpkgs locks (2025.12.4), not from the documentation page:
#
#   name            `mcp_server`. nixpkgs knows it: it is in
#                   pkgs/servers/home-assistant/component-packages.nix's
#                   supported list, and its entry pulls `aiohttp-sse`, `anyio`,
#                   `hassil`, `home-assistant-intents` and `mcp` into HA's
#                   python environment. So `extraComponents` is all that is
#                   needed to make it importable -- there is nothing to
#                   package and no custom component.
#
#   endpoints       homeassistant/components/mcp_server/http.py, verbatim:
#                     SSE_API        = f"/{DOMAIN}/sse"
#                     MESSAGES_API   = f"/{DOMAIN}/messages/{{session_id}}"
#                     STREAMABLE_API = "/api/mcp"
#                   i.e. `/mcp_server/sse` for the SSE transport (the client
#                   opens this and is told the messages URL to POST to), and
#                   `/api/mcp` for the newer streamable-HTTP transport. A
#                   client configured with the vhost root, or with `/api/mcp`
#                   when it speaks SSE, gets a 404 and no explanation.
#
#   auth            A long-lived access token as `Authorization: Bearer <t>`.
#                   None of the three views sets `requires_auth = False`, and
#                   HomeAssistantView defaults it to True; on top of that each
#                   one calls `_validate_admin()`, which checks
#                   `request["hass_user"].is_admin` when the config entry was
#                   created with `require_admin`. So the token's HA user must
#                   be an admin if that box was ticked. With ./auth.nix's
#                   `roles.admin = "idm_admins"`, a kanidm account in
#                   `idm_admins` is one.
#
#   config          THERE IS NO YAML FORM, and adding one is actively harmful.
#                   The integration's __init__.py sets
#                   `CONFIG_SCHEMA = cv.config_entry_only_config_schema(DOMAIN)`,
#                   and that helper's validator in
#                   homeassistant/helpers/config_validation.py is
#                   `if domain in config: <log error>; raise_issue()` -- it
#                   tests PRESENCE, not contents, so even `mcp_server: {}`
#                   logs "The mcp_server integration does not support YAML
#                   setup" and raises a repair issue. Hence `extraComponents`
#                   below and deliberately nothing in
#                   `services.home-assistant.config`.
#
#                   The manifest is `config_flow: true`,
#                   `single_config_entry: true`, `dependencies: ["http",
#                   "conversation"]`. `http` and `conversation` are already
#                   satisfied -- ./core.nix configures `http` and
#                   `default_config` brings `conversation` -- so no `api:`
#                   entry and no extra `http` key is required. The config
#                   entry itself is created once, in the UI, and it is where
#                   the LLM API (Assist) and `require_admin` are chosen.
#
# --- what else must happen first ---
#
#   1. The config entry. Settings -> Devices & Services -> Add Integration ->
#      "Model Context Protocol", pick the LLM API to expose (Assist) and
#      decide `require_admin`. `single_config_entry: true`, so there is
#      exactly one and the "Add" button disappears afterwards.
#
#   2. A long-lived access token for whatever will connect. There is NO
#      declarative mint for one in this repo today -- the same fact
#      ./state.nix records about `.storage` holding "every device, entity,
#      person and long-lived access token", because that registry is the only
#      copy. The token is minted over HA's websocket API
#      (`auth/long_lived_access_token`, which is
#      `@websocket_api.ws_require_user()`-gated and so needs an already
#      authenticated session), which is what makes an agenix generator for it
#      possible but not what makes it exist: that generator is being built
#      separately and is also off. Until it lands, the token comes out of the
#      HA profile page by hand and goes into an agenix `rekeyFile` secret
#      beside whichever CLIENT needs it -- which is a different aspect from
#      this one. Nothing here reads it, so nothing here breaks while it is
#      absent; the endpoint simply 401s.
#
#      Two properties of HA's tokens worth knowing before minting more than
#      one: the lifespan is 10 years, and re-minting ORPHANS the previous
#      token server-side rather than replacing it, so stale ones accumulate in
#      `.storage` and have to be revoked through the profile UI or
#      `/auth/revoke`.
#
# --- how it is reached ---
#
# Through ./vhost.nix's `home.<domain>` leg, which already proxies `/` with
# `proxy_buffering off` -- which the SSE transport needs and which is why no
# separate location is required here. A client on this host should use
# `http://127.0.0.1:8123/mcp_server/sse` instead, for the reason
# ../nextcloud.nix:327 and ../../hosts/secondpc/sso.nix:385 give: the public
# name puts DNS, TLS and the cloudflare tunnel on the path of a call between
# two processes on one box.
{
  den.aspects.homeassistant-mcp = {
    # So this cannot be included on a host that does not run HA: including it
    # brings the whole service with it rather than silently setting an option
    # on a disabled module.
    includes = [ den.aspects.homeassistant ];

    nixos.services.home-assistant.extraComponents = [ "mcp_server" ];
  };
}
