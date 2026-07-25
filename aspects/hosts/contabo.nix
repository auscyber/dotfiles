{ den, ... }: {
  den.homes.x86_64-linux."ivy@contabo" = {
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
