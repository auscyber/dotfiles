{ inputs, ... }:
{
  ff.nixos-facter-modules.url = "github:nix-community/nixos-facter-modules";

  # Opt-in per host: a host that includes this aspect must set
  # `hardware.facter.reportPath` (pointing at its facter.json).
  den.aspects.facter.nixos.imports = [ inputs.nixos-facter-modules.nixosModules.facter ];
}
