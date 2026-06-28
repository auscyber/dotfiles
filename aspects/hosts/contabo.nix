{ den, ... }:
{
  den.hosts.x86_64-linux.contabo = {
    users.ivy = { };
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
