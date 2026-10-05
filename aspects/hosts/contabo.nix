{ den, ... }: {
  den.homes.x86_64-linux."ivy@contabo" = {
    # The human behind this account (../persons/ivy.nix). The link points
    # account -> person, never the reverse; ../base/person.nix says why.
    person = den.persons.ivy;
  };

  den.aspects.contabo = { };

  den.aspects.ivy.provides.contabo = {
    includes = [
      den.aspects.fish
      den.aspects.neovim
      den.batteries.primary-user
    ];
    homeManager.targets.genericLinux.enable = true;
  };
}
