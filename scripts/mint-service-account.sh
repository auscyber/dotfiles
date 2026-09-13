#!/usr/bin/env bash
# Mints a new gateway service account, or grants an existing one another
# service to call, by editing aspects/services/service-accounts.json --
# the registry aspects/services/service-accounts.nix turns into
# gateway.serviceAccounts.<name> plus <name> in that service's
# gateway.services.<target>.api.clients.
#
# Usage:
#   scripts/mint-service-account.sh <name> <description> <target>...
#
# Example:
#   scripts/mint-service-account.sh alloy-remote \
#     "alloy on <otherhost>, pushing traces to tempo" tempo
#
# Re-run with the same name to add more targets or update the description --
# it merges rather than duplicating.
set -euo pipefail

repo="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
registry="$repo/aspects/services/service-accounts.json"

if [ "$#" -lt 3 ]; then
  echo "usage: $0 <name> <description> <target-service>..." >&2
  exit 1
fi

name="$1"
description="$2"
shift 2
targets=("$@")

if ! [[ "$name" =~ ^[a-z][a-z0-9-]*$ ]]; then
  echo "error: name '$name' must be lowercase, start with a letter, and use only [a-z0-9-]" >&2
  echo "(it becomes a systemd/nginx/env identifier downstream)" >&2
  exit 1
fi

if [ ! -f "$registry" ]; then
  echo "error: $registry not found" >&2
  exit 1
fi

targets_json="$(printf '%s\n' "${targets[@]}" | jq -R . | jq -s .)"

tmp="$(mktemp)"
trap 'rm -f "$tmp"' EXIT

jq \
  --arg name "$name" \
  --arg description "$description" \
  --argjson newTargets "$targets_json" \
  '
    .accounts[$name].description = $description
    | .accounts[$name].clientOf = ((.accounts[$name].clientOf // []) + $newTargets | unique | sort)
    | .accounts |= (to_entries | sort_by(.key) | from_entries)
  ' \
  "$registry" >"$tmp"

mv "$tmp" "$registry"

echo "minted/updated '$name' -> ${targets[*]} in $registry"
echo
echo "next:"
echo "  1. review the diff (jj diff -- $registry, or git diff)"
echo "  2. build and switch the host(s) that include den.aspects.service-accounts"
echo "     (secondpc today) -- this is what actually generates the key:"
echo "       sudo nixos-rebuild switch --flake .#<host>"
echo "  3. the raw key only ever exists in plaintext on that host. Once switched:"
echo "       sudo cat /run/agenix/secrets/$name/api-key"
echo "     hand that to whatever is calling in as '$name'."
