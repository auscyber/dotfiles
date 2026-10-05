#!/usr/bin/env bash
# Regenerate every committed capability graph. Run from anywhere:
#
#   ./docs/capability-graphs/regen.sh
#
# Flake-level only: no NixOS config is built, no host fixpoint is forced, and
# nothing here needs the eval lock. See ./README.md for the measured cost.
set -euo pipefail

root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
out="$root/docs/capability-graphs"
expr="$out/render.nix"

# `nix eval -f` does NOT auto-call a function the way nix-build does: it
# evaluates the file and hands back the lambda ("cannot coerce a function to a
# string"). `--apply` applies it explicitly, which is also where the arguments
# go.
render() {
  local host="$1" view="$2" file="$3" focus="${4:-null}" extra="${5:-}"
  # `${5:-{ }}` does NOT work here: bash ends the parameter expansion at the
  # first `}`, so the default became `{ ` with a stray `}` appended after the
  # substitution -- which produced `reachOverride = { mosquitto = "host"; }}`
  # and a nix syntax error only on the calls that passed one.
  [ -n "$extra" ] || extra="{ }"
  nix eval --impure --raw -f "$expr" --apply \
    "f: f { root = \"$root\"; host = \"$host\"; view = \"$view\"; focus = $focus; reachOverride = $extra; }" \
    >"$out/$file"
  echo "  $file"
}

echo "capability graphs -> $out"

# ── THE RESOLUTION SLICE, PER HOST ──
# The readable view: every take on the host, the giver that won, the carrier it
# won over, and the candidates that lost with the clause they lost on.
for h in secondpc auspc; do
  render "$h" slice "$h-capabilities.mmd"
  render "$h" report "$h-resolutions.md"
done

# ── THE WHOLE HOST GRAPH, WITH THE CAPABILITY LAYER IN IT ──
# One host only. This is what `apps.docs` renders for every host once
# den-diagram is locked; committed here for auspc so the integration is
# visible without building anything.
render auspc full "auspc-full.mmd"

# ── THE REACH WHAT-IF ──
# ../../aspects/services/homeassistant/mosquitto.nix was `reach = "host"` until
# recently, and from auspc that returned null with nothing to look at. This
# renders the same take with mosquitto pretended back to `host`, so the shape
# of a reach failure is legible beside the resolution that replaced it.
render auspc slice "example-mqtt-reach-whatif.mmd" '"corsair-psu.mqtt"' '{ mosquitto = "host"; }'
render auspc report "example-mqtt-reach-whatif.md" '"corsair-psu.mqtt"' '{ mosquitto = "host"; }'

# ── THE FOUR WORKED EXAMPLES, ONE TAKE EACH ──
#
#   openrgb.home-assistant   cross-host, reach = household, NOT routed, so the
#                            resolution ends at the giver with no carrier.
#   vban-turntable.udp       a take with CLAUSES: the tunnel loses on `mtu`
#                            rather than by exclusion.
#   corsair-psu.mqtt         the same capability that used to fail on reach,
#                            now resolving household-wide over the lan.
#   homeassistant.mqtt       same host, so `loopback 0 (same host)` rather
#                            than a wire.
#   paperwork.share          an UNRESOLVED take, from the host it fails on.
render auspc    slice  "example-openrgb-home-assistant.mmd" '"openrgb.home-assistant"'
render auspc    slice  "example-vban-udp.mmd"               '"vban-turntable.udp"'
render auspc    report "example-vban-udp.md"                '"vban-turntable.udp"'
render auspc    slice  "example-corsair-mqtt.mmd"           '"corsair-psu.mqtt"'
render secondpc slice  "example-homeassistant-mqtt.mmd"     '"homeassistant.mqtt"'
render auspc    slice  "example-paperwork-share-unresolved.mmd" '"paperwork.share"'

echo "done"
