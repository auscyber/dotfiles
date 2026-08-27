#!/usr/bin/env bash
#
# Turn nvim-lspconfig into an attrset (or JSON) for the lspmux `lsp-servers`
# class: for every `lsp/<name>.lua` upstream ships, the cmd it spawns the server
# with -- split into `exe` + `args`, flagged `dynamicCmd` when lspconfig builds
# the cmd in a lua function at runtime -- and the filetypes it claims.
#
# It is the generator half of checks.lspmux-server-names
# (aspects/editors/lspmux/lspmux-check.nix): this writes down what upstream says,
# that check keeps the hand-written table from drifting from it afterwards.
#
# Nothing is scraped out of the source. The configs are lua, so they are
# evaluated: loaded inside `nvim --headless -l`, and where `cmd` is a function,
# *called* with `vim.lsp.rpc.start`/`connect` stubbed out so the argv it was
# about to spawn can be read off instead of run. `extensionToLanguage` comes off
# nvim's own filetype table the same way.
#
# The reader's machine is kept out of the output by evaluating in a fake world:
# HOME, the XDG dirs and TMPDIR are planted with dummy paths and the globals with
# no env var behind them (vim.fn.tempname, vim.fn.getcwd, vim.uv.os_homedir,
# vim.uv.os_tmpdir) are overwritten to match, so a path a config builds can be
# replaced straight back out -- powershell_es' log path comes out as
# `@XDG_CACHE_HOME@/nvim/powershell_es.log`, a token to substitute (the entry
# lists them under `placeholders`). Each config is evaluated twice in two such
# worlds; what still differs is derived from something we could not plant (the
# project root, a tempname) and is dropped with a note instead of frozen in.
#
# What is NOT generated: `package`, `zed`, `opencode`, `nvim` -- nvim-lspconfig
# has no opinion on the nixpkgs attr or on another editor's vocabulary. Merge
# the generated spawn contract with those by hand.
#
# Also wired as a flake app -- `nix run .#gen-lsp-servers -- <flags>` -- which
# pins both nvim and nvim-lspconfig instead of resolving them here; see
# aspects/editors/lspmux/gen-servers.nix.
#
#   scripts/gen-lsp-servers.sh                                  # all servers, JSON
#   scripts/gen-lsp-servers.sh --format nix --only pyright,gopls
#   scripts/gen-lsp-servers.sh --format nix -o lsp-servers.nix
#   scripts/gen-lsp-servers.sh --src ~/src/nvim-lspconfig       # a checkout instead
#
# Flags: --src DIR, --format json|nix, --only a,b,c, -o FILE, --no-extensions,
#        --fields exe,args,dynamicCmd,cmd,filetypes,extensionToLanguage,
#                 placeholders,notes
#        (--fields exe,args,dynamicCmd is the subset lspSubModule declares -- it
#        has no `filetypes` option, so a generated entry with one merged in
#        as-is would not evaluate).
#
# Without --src (or $NVIM_LSPCONFIG) it builds vimPlugins.nvim-lspconfig from
# this flake's own locked nixpkgs, which costs a flake eval; pass a checkout or a
# store path when iterating.
#
set -euo pipefail

here=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
lua=$here/gen-lsp-servers.lua

src=${NVIM_LSPCONFIG:-}
format=json
out=
lua_args=()

usage() {
  sed -n '3,50p' "${BASH_SOURCE[0]}" | sed 's/^# \{0,1\}//'
}

while [ $# -gt 0 ]; do
  case $1 in
    --src) src=$2; shift 2 ;;
    --format) format=$2; shift 2 ;;
    --only) lua_args+=(--only "$2"); shift 2 ;;
    --fields) lua_args+=(--fields "$2"); shift 2 ;;
    --no-extensions) lua_args+=(--no-extensions); shift ;;
    -o | --out) out=$2; shift 2 ;;
    -h | --help) usage; exit 0 ;;
    *) echo "unknown argument: $1" >&2; usage >&2; exit 2 ;;
  esac
done

case $format in
  json | nix) ;;
  *) echo "--format must be json or nix" >&2; exit 2 ;;
esac

# No --src: build the same plugin the configuration ships, pinned to this
# flake's own locked nixpkgs. The lock is read directly rather than going through
# `--inputs-from`, which would copy the whole worktree into the store first.
resolve_locked_nixpkgs() {
  local expr='let
      lock = builtins.fromJSON (builtins.readFile "'"$1"'/flake.lock");
      ref = lock.nodes.root.inputs.nixpkgs or null;
      locked = if builtins.isString ref then lock.nodes.${ref}.locked else null;
    in
    if locked != null && (locked.type or "") == "github"
    then "github:${locked.owner}/${locked.repo}/${locked.rev}"
    else ""'
  nix eval --raw --impure --expr "$expr" 2>/dev/null
}

if [ -z "$src" ]; then
  command -v nix >/dev/null || {
    echo "no --src given and no nix on \$PATH to build vimPlugins.nvim-lspconfig" >&2
    exit 1
  }
  flake=$(git -C "$here" rev-parse --show-toplevel 2>/dev/null || echo "$here/..")
  nixpkgs=$(resolve_locked_nixpkgs "$flake")
  echo "building vimPlugins.nvim-lspconfig from ${nixpkgs:-the locked nixpkgs}..." >&2
  if [ -n "$nixpkgs" ]; then
    src=$(nix build --no-link --print-out-paths "$nixpkgs#vimPlugins.nvim-lspconfig")
  else
    src=$(nix build --no-link --print-out-paths --inputs-from "$flake" nixpkgs#vimPlugins.nvim-lspconfig)
  fi
fi

[ -d "$src/lsp" ] || {
  echo "$src is not an nvim-lspconfig checkout (no lsp/ directory)" >&2
  exit 1
}

# The reader's own nvim must not colour what upstream says -- and a nix-wrapped
# nvim injects its plugins with `--cmd "set packpath^=..."` ahead of our args, so
# `--clean` alone still leaves them loaded (they then race the extraction: a
# copilot autostart lands in the stubbed rpc entry points). Pinning packpath and
# rtp back to $VIMRUNTIME, after the wrapper's own --cmd, leaves plain nvim.
nvim_args=(
  --headless --clean
  --cmd 'set packpath=$VIMRUNTIME'
  --cmd 'set runtimepath=$VIMRUNTIME'
  -l "$lua" --src "$src" --format "$format"
)

run_nvim() {
  if command -v nvim >/dev/null; then
    nvim "${nvim_args[@]}" "${lua_args[@]}"
  else
    nix shell --inputs-from "${flake:-$here/..}" nixpkgs#neovim \
      --command nvim "${nvim_args[@]}" "${lua_args[@]}"
  fi
}

if [ -n "$out" ]; then
  run_nvim > "$out"
  echo "wrote $out" >&2
else
  run_nvim
fi
