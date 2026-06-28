{ den, ... }:
{
  den.aspects.nushell = {
    includes = [ den.aspects.shell ];
    homeManager = {
      programs.nushell.enable = true;
    };
  };
}
