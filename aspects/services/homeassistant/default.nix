{ den, ... }:
# Home Assistant, native on the host per https://wiki.nixos.org/wiki/Home_Assistant.
#
# This used to be HAOS as a NixVirt-declared libvirt domain (`_haos.nix` plus
# `_bridged-network.nix` / `_default-network.nix`, all removed), with the
# supervisor owning mosquitto, matter-server, esphome and Music Assistant as
# add-ons inside the VM. Native HA has no supervisor, so each of those is a host
# service here and the `hassio` integration simply ceases to exist.
#
# The integration list in ./core.nix is what the HAOS instance actually had --
# read off its `.storage/core.config_entries` -- minus the two that cannot
# survive the move: `hassio` (supervisor) and `hacs` (installs components
# imperatively into the config dir, which is what `customComponents` replaces).
#
# --- the files in this directory ---
#
# One aspect, `den.aspects.homeassistant`, written from several files; den
# merges `nixos` class content across them as modules, which is the same thing
# ../../hosts/secondpc/{default,media,observability,vaultwarden}.nix already do
# for `den.aspects.secondpc`.
#
#   ./core.nix             the HA service itself: components, the recorder, the
#                          YAML `!include`s, `http`, its own firewall openings.
#   ./auth.nix             kanidm as a login provider (the OIDC client and
#                          auth_oidc).
#   ./vhost.nix            home.<domain>, which is deliberately NOT gated.
#   ./state.nix            the recorder's database and everything restic has to
#                          carry off the box.
#   ./mosquitto.nix        ex-add-on: the MQTT broker.
#   ./matter.nix           ex-add-on: the Matter fabric controller.
#   ./esphome.nix          ex-add-on: the ESPHome device builder dashboard.
#   ./music-assistant.nix  ex-add-on: Music Assistant.
#   ./mcp-server.nix       HA as an MCP server. NOT INCLUDED -- a separate,
#                          unreferenced aspect; see that file's header.
#   ./prom-device.nix      node_exporter metrics as HA devices. Its own aspect
#                          (`node-metrics`), attached to every host.
{
  den.aspects.homeassistant.includes = [
    # nginx + ACME (wildcard ivymect.in) + the cloudflare tunnel that already
    # fronts every other *.ivymect.in vhost on this box.
    den.aspects.secondpc-web
    # `gated`, for the SSO-fronted esphome and Music Assistant vhosts.
    den.aspects.gateway
    # The `postgres` quirk, which owns the server and collects every
    # service's database. See ../postgres.nix.
    den.aspects.postgres
    # ../restic.nix's `backup` quirk, which carries the state declared in
    # ./state.nix off the box. The recorder is on postgres, so it is already
    # covered by that aspect's dump of every provisioned database; what
    # ./state.nix declares is everything HA keeps OUTSIDE a database.
    den.aspects.restic
  ];
}
