#!/usr/bin/env bash
#
# Regenerate a host's kernel "trim": the set of kernel config symbols that
# `make localmodconfig` switches off once the module list is narrowed to what
# this machine actually loads.
#
# The output is a plain symbol -> value map at
# aspects/hosts/<host>/kernel-trim.json, consumed by
# aspects/nixos/kernels/trim.nix as a `boot.kernelPatches` entry. It is a DIFF
# against the host's own kernel, not a frozen .config: nixpkgs still assembles
# the config (common-config, the host's other kernelPatches, NixOS
# requirements) and this only subtracts from the result. That is what keeps it
# working across kernel bumps and across kernels — cachyos, zen, vanilla — since
# the base is always whatever `boot.kernelPackages` already resolves to.
#
# Run this ON the target host (it reads that machine's modprobed.db) from the
# repo root:
#
#   nix run .#gen-kernel-trim
#
# Prerequisites:
#   * the host includes `den.aspects.kernel-trim` (that is what provides the
#     `system.build.kernelLocalmodconfig` builder this script drives);
#   * `modprobed-db` has been collecting for a while — ideally across a boot or
#     two with every peripheral plugged in. Anything missing from it at
#     generation time gets its driver compiled out.
#
# Environment overrides: HOST, FLAKE_REF, MODPROBED_DB, OUT.
set -euo pipefail

HOST="${HOST:-$(uname -n)}"
FLAKE_REF="${FLAKE_REF:-.}"
MODPROBED_DB="${MODPROBED_DB:-${XDG_CONFIG_HOME:-$HOME/.config}/modprobed.db}"
OUT="${OUT:-$PWD/aspects/hosts/$HOST/kernel-trim.json}"

CFG="${FLAKE_REF}#nixosConfigurations.${HOST}.config"

if [[ ! -r $MODPROBED_DB ]]; then
  echo "gen-kernel-trim: no modprobed.db at $MODPROBED_DB" >&2
  echo "  Add pkgs.modprobed-db, run 'modprobed-db store' (and let its timer run" >&2
  echo "  across a few boots) before trimming, or point MODPROBED_DB elsewhere." >&2
  exit 1
fi

work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT

echo "==> collecting the modules $HOST declares"
# modprobed.db only knows what has been *loaded*. The initrd lists (which on
# these hosts come from facter) cover the ones needed to reach the root device,
# some of which are built in or loaded before anything is recording.
nix eval --json "$CFG" --apply \
  'c: c.boot.initrd.availableKernelModules ++ c.boot.initrd.kernelModules ++ c.boot.kernelModules' |
  jq -r '.[]' >"$work/declared"

# streamline_config.pl accepts lsmod output or a bare list of module names; take
# the first field of each line so both forms work. Hyphen/underscore spelling is
# normalised by localmodconfig itself.
awk 'NF && $1 !~ /^#/ { print $1 }' "$MODPROBED_DB" "$work/declared" |
  tr -d '\r' | sort -u >"$work/lsmod"
echo "    $(wc -l <"$work/lsmod") modules to keep"

echo "==> building base + trimmed kernel config"
# `nix build` has no --apply, so instantiate via `nix eval` and build the .drv.
# --impure is needed only for `builtins.path` on the temp file above.
drv=$(nix eval --impure --raw "$CFG.system.build.kernelLocalmodconfig" \
  --apply "f: (f (builtins.path { name = \"lsmod\"; path = \"$work/lsmod\"; })).drvPath")
configs=$(nix build --no-link --print-out-paths "$drv^out")

echo "==> diffing $configs/{base,trimmed}.config"
awk -v base="$configs/base.config" -v trimmed="$configs/trimmed.config" '
  function load(file, arr,   line, key, val, eq) {
    while ((getline line < file) > 0) {
      if (line ~ /^CONFIG_[A-Za-z0-9_]+=/) {
        eq = index(line, "=")
        key = substr(line, 8, eq - 8)
        val = substr(line, eq + 1)
        # Strings are quoted in .config; kernel_config.nix re-quotes freeform
        # values itself, so store them bare.
        if (val ~ /^".*"$/) val = substr(val, 2, length(val) - 2)
        arr[key] = val
      } else if (line ~ /^# CONFIG_[A-Za-z0-9_]+ is not set$/) {
        key = line
        sub(/^# CONFIG_/, "", key)
        sub(/ is not set$/, "", key)
        arr[key] = "n"
      }
    }
    close(file)
  }
  BEGIN {
    load(base, b)
    load(trimmed, t)
    for (key in t) {
      old = (key in b) ? b[key] : "n"
      if (t[key] != old) print key "\t" t[key]
    }
  }
' | sort >"$work/delta"

count=$(wc -l <"$work/delta")
disabled=$(awk -F'\t' '$2 == "n"' "$work/delta" | wc -l)
echo "    $count symbols changed ($disabled disabled)"

if [[ $count -eq 0 ]]; then
  echo "gen-kernel-trim: nothing to trim — leaving $OUT alone" >&2
  exit 0
fi

mkdir -p "$(dirname "$OUT")"
jq -Rn '[inputs | split("\t") | { key: .[0], value: .[1] }] | from_entries' \
  <"$work/delta" >"$OUT"

echo "wrote $OUT"
echo "The aspect enables itself as soon as this file exists; 'jj status' it so"
echo "the flake copy can see it, then rebuild."
