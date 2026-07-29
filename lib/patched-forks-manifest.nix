# Manifest of which patched flake inputs to publish as GitHub forks.
#
# Pure builtins — no nixpkgs, no flake, no module system — so CI can evaluate it
# with a single
#     nix eval --json --expr 'import ./lib/patched-forks-manifest.nix { ... }'
# without instantiating the whole flake (fast, hermetic, no IFD).
#
# Source of truth: the committed ../patched-inputs.nix (the exact `isInput`
# patch set flake.nix itself builds from) crossed with ../flake.lock (the
# upstream github owner/repo/rev each of those inputs is currently pinned to).
# Only github-type inputs that actually carry patches are emitted; package-only
# overlay patches (nixpkgs, jankyborders, …) and hashless no-op entries are
# skipped — we fork whole flake inputs, not individual packages.
#
# Each entry says: take `<upstreamOwner>/<upstreamRepo>` at `rev`, apply
# `patches` on top, and push the result to `<targetOwner>/<targetRepo>` on
# `branch`. See ../scripts/push-patched-forks.sh for the consumer.
{
  # GitHub account that owns the destination forks.
  owner ? "auscyber",
  # Branch pushed to each fork.
  branch ? "dendritic-patched",
  # Flake root (holds patched-inputs.nix and flake.lock). Defaults to the
  # repo root relative to this file, so `import ./lib/patched-forks-manifest.nix {}`
  # just works from the flake root.
  root ? ../.,
}:
let
  patched = import (root + "/patched-inputs.nix");
  lock = builtins.fromJSON (builtins.readFile (root + "/flake.lock"));
  rootInputs = lock.nodes.${lock.root}.inputs or { };

  # Resolve a root-input ref (a node-key string, or a ["a" "b"] follows path) to
  # a node key, mirroring flake.lock's own resolution.
  resolveRef = ref: if builtins.isList ref then resolvePath lock.root ref else ref;
  resolvePath =
    node: path:
    if path == [ ] then
      node
    else
      resolvePath (resolveRef lock.nodes.${node}.inputs.${builtins.head path}) (builtins.tail path);

  # Make a patch path repo-relative (e.g. "patches/agenix/edit.patch") so the
  # push script can read it from the checkout. Coerce with toString first —
  # never let a bare path reach toJSON, which would copy it into the store.
  rootStr = builtins.toString root;
  prefix = rootStr + "/";
  plen = builtins.stringLength prefix;
  relTo =
    p:
    let
      s = builtins.toString p;
    in
    if builtins.substring 0 plen s == prefix then
      builtins.substring plen (builtins.stringLength s) s
    else
      s;

  entryFor =
    name:
    let
      spec = patched.${name};
      patches = spec.patches or [ ];
      key = resolveRef (rootInputs.${name} or name);
      locked = (lock.nodes.${key} or { }).locked or { };
      isGithub = (locked.type or null) == "github";
    in
    if isGithub && patches != [ ] then
      [
        {
          inherit name branch;
          upstreamOwner = locked.owner;
          upstreamRepo = locked.repo;
          rev = locked.rev;
          targetOwner = owner;
          # Keep the fork's name identical to upstream (input name may differ,
          # e.g. input `darwin` ← repo `nix-darwin`).
          targetRepo = locked.repo;
          patches = map relTo patches;
        }
      ]
    else
      [ ];
in
builtins.concatLists (map entryFor (builtins.attrNames patched))
