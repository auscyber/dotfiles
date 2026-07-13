{ ... }:
{
  # Opt-in per host: a host that includes this aspect must set
  # `hardware.facter.reportPath` (pointing at its facter.json).
  #
  # Hardware detection comes from nixpkgs' own `hardware/facter` module, which is
  # in the NixOS module list — that is what owns `hardware.facter.*`. We used to
  # also import nixos-facter-modules here, but it declares its options under a
  # separate `facter.*` namespace (which nothing here sets, so it was inert) while
  # *also* declaring `system.build.{noFacter,nvd,nix-diff}`. Having both meant
  # those options had two declarations, which errors as soon as anything forces
  # them — nothing in a normal build does, but rendering the option docs
  # (aspects/docs/_searchix-sources.nix) forces every option and trips over it.
  den.aspects.facter = { };
}
