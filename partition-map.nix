# Which aspects live in a flake-parts partition instead of the base evaluation.
#
# An aspect listed here is NOT imported by the root flake, so its `ff.*` input
# declarations never reach the root `flake.nix` or `flake.lock`. They are written
# to `partitions/<bucket>/flake.nix` instead -- also by `nix run .#write-flake` --
# and merged back in only when a partitioned output attribute is evaluated. See
# aspects/framework/partitions.nix for the wiring and `partitionedAttrs` for which
# flake outputs come from the partition.
#
# Entries are paths relative to ./aspects. A directory entry claims everything
# under it.
#
# Adding a partitioned input is a two-step dance, for the same reason
# patched-inputs.nix is generated: the partition can only be evaluated once its
# sub-flake already provides the input, and the sub-flake is written by evaluating
# the partition. Declare `ff.<name>` first, run `nix run .#write-flake` and
# `nix flake lock ./partitions/<bucket>`, then write the code that uses
# `inputs.<name>`.
{
  # homebrew.nix owns `den.aspects.homebrew` and the generated tap inputs; the
  # aspects that `include` it have to come along or the base evaluation cannot
  # resolve them. Both hosts and zotero are Darwin-only anyway.
  darwin = [
    "darwin/homebrew.nix"
    "programs/zotero.nix"
    "hosts/laptop.nix"
    "hosts/macmini.nix"
  ];

  dev = [
    "tooling/deploy.nix"
    "tooling/ci-matrix.nix"
    "docs/default.nix"
  ];
}
