#!/usr/bin/env bash
# Rebuild the master-encrypted secret sources after losing the master identity.
#
# WHY THIS EXISTS
#   `secrets/static/*.age` are encrypted to the master identity ONLY. When that
#   identity became unusable, every one of them became unreadable -- and
#   `agenix rekey` could not help, because rekeying DECRYPTS the static sources
#   in order to re-encrypt them per host. The way out is that agenix-rekey also
#   writes `secrets/rekeyed/<host>/...`, encrypted to each host's own SSH host
#   key, which we still control. Those copies are the recovery material.
#
# WHAT IT DOES
#   For each secret below: decrypt the host copy with that host's private host
#   key and re-encrypt it to the NEW masters, straight through a pipe. The
#   plaintext never reaches disk and never appears in a process argument.
#   Each rebuilt file is verified decryptable before it replaces anything.
#
# WHAT IT CANNOT DO
#   Only secrets that were actually DEPLOYED to a host have a rekeyed copy.
#   The ones listed under "NOT RECOVERABLE" at the bottom have none and must be
#   re-issued from the provider or retyped.
set -euo pipefail

cd "$(dirname "$0")/../.."
[ -e flake.nix ] || { echo "run from the repo (flake.nix not found)" >&2; exit 1; }

# The new masters, from ./agenix-rekey.nix: the enclave key is the
# `masterIdentities` entry, the backup is `extraEncryptionPubkeys` (agenix's own
# option for "encrypt to this too, but never try to decrypt with it"). This
# script encrypts to BOTH explicitly, so a file it rebuilds is readable by
# either one -- the same set agenix itself will use from here on.
SE_RECIPIENT="age1se1qtu7ycpgpl79gzdflwuywxc6qttaqldmyvv3w8yqhq5m7alk2fqd2cht23t"
BACKUP_RECIPIENT="$(tr -d '\n' < aspects/security/backup-master.pub)"
SE_IDENTITY="$HOME/Library/agenix/se-identity.txt"

LOCAL_HOSTKEY=/etc/ssh/ssh_host_ed25519_key
SECONDPC=root@192.168.0.26
REMOTE_HOSTKEY=/etc/ssh/ssh_host_ed25519_key

# dest|host|rekeyed copy
#
# `local` means this Mac, whose own host key is the recipient -- no SSH, just
# sudo. Prefer it where a secret was deployed to both.
ENTRIES=(
  "secrets/static/wakatime_api.age|local|secrets/rekeyed/Ivys-MacBook-Pro/a8e6c075fabc12b9d87543f7660e07fd-wakatime/wakatime.age"
  "secrets/static/tailscale_auth.age|local|secrets/rekeyed/Ivys-MacBook-Pro/66d676d934195e02f519bf78b1f67d2b-tailscale/auth.age"
  "secrets/static/github_token.age|local|secrets/rekeyed/Ivys-MacBook-Pro/f2ba0c704bc412d53de06bf06bf729b5-dev-nix/nix_github_token.age"
  "secrets/static/fargonekey.age|local|secrets/rekeyed/Ivys-MacBook-Pro/5925a8b0f2696972bb8eebe624060803-builder-ssh-key/fargonekey.age"
  "aspects/dev/llms/token.age|local|secrets/rekeyed/Ivys-MacBook-Pro/2ba30abfc18413235b14bd93a3ac8412-claude/claude_token.age"
  "aspects/dev/llms/other_token.age|local|secrets/rekeyed/Ivys-MacBook-Pro/a53ef35306bc2bce3c6938a7df3d1287-claude/dad_token.age"
  "secrets/static/cloudflare_api.age|secondpc|secrets/rekeyed/secondpc/b9a8c7317460ddfa41cccb34af050877-cloudflare/token.age"
  "aspects/base/github_cache_key.age|secondpc|secrets/rekeyed/secondpc/98a4806b9883c4af91000ace78305a36-celler/github_cache_key.age"
)

echo "Rebuilding to:"
echo "  enclave: $SE_RECIPIENT"
echo "  backup : $BACKUP_RECIPIENT"
echo

ok=0 failed=0
for e in "${ENTRIES[@]}"; do
  IFS='|' read -r dest host src <<<"$e"
  printf '%-44s ' "$(basename "$dest")"

  if [ ! -e "$src" ]; then echo "SKIP (no rekeyed copy at $src)"; failed=$((failed+1)); continue; fi

  tmp="$(mktemp "${TMPDIR:-/tmp}/recov.XXXXXX")"
  # Re-encrypt straight out of the decryption pipe. `set -o pipefail` above is
  # what makes a failed decrypt fail the whole thing instead of silently
  # producing an encrypted empty file.
  if [ "$host" = local ]; then
    sudo rage -d -i "$LOCAL_HOSTKEY" "$src" \
      | rage -e -r "$SE_RECIPIENT" -r "$BACKUP_RECIPIENT" -o "$tmp" || { echo "FAILED (decrypt)"; rm -f "$tmp"; failed=$((failed+1)); continue; }
  else
    # `cat` the file in over stdin so the remote never needs the repo.
    ssh "$SECONDPC" "rage -d -i $REMOTE_HOSTKEY" < "$src" \
      | rage -e -r "$SE_RECIPIENT" -r "$BACKUP_RECIPIENT" -o "$tmp" || { echo "FAILED (decrypt)"; rm -f "$tmp"; failed=$((failed+1)); continue; }
  fi

  # Non-empty, and actually readable by the new master, BEFORE it replaces the
  # only copy we have. Touch ID will prompt here.
  if [ ! -s "$tmp" ]; then echo "FAILED (empty)"; rm -f "$tmp"; failed=$((failed+1)); continue; fi
  if ! rage -d -i "$SE_IDENTITY" "$tmp" >/dev/null 2>&1; then
    echo "FAILED (new file does not decrypt)"; rm -f "$tmp"; failed=$((failed+1)); continue
  fi

  mkdir -p "$(dirname "$dest")"
  mv "$tmp" "$dest"
  echo "ok"
  ok=$((ok+1))
done

echo
echo "recovered: $ok   failed/skipped: $failed"
cat <<'EOF'

NOT RECOVERABLE -- no rekeyed copy exists on any host, so these must be
re-issued from the provider or retyped with `nix run .#secret-edit -- <path>`:

  secrets/static/kagi_token.age            Kagi API token
  secrets/static/wifi_password.age
  secrets/static/bees_wifi.age             birdsarentreal was never rekeyed
  secrets/static/claude_token.age          zeroclaw's copy (not deployed); the
                                           recovered aspects/dev/llms/token.age
                                           is likely the same value
  secrets/static/rclone/gdrive.age
  secrets/static/rclone/onedrive-personal.age
  secrets/static/zotero/webdav.age
  secrets/static/ivy_password.age          intermediary, so it was NEVER
                                           deployed; only its one-way
                                           derivatives (sha512-crypt, NT hash)
                                           are on hosts. Choose a new password.

THEN:
  nix run .#rekey          re-encrypt every static source to every host key
  nix run .#gen-secrets    regenerate all generated secrets

Regenerating generated secrets is NOT free -- read before running:
  * vpn-secrets/wireguard_key  a new keypair breaks the tunnel until EVERY
                               peer is redeployed. Do the fleet in one go.
  * codesign/identity          a new signing identity invalidates the TCC
                               grants on this Mac; aspects/hosts/laptop.nix
                               documents the comment-out-wrapperd dance.
  * celler tokens              every host must be redeployed to pick up new
                               ones, or pushes/pulls 401.
  * sso/*, grafana, *-api-key  safe; kanidm and the services are reprovisioned
                               from these on the next activation.
EOF
