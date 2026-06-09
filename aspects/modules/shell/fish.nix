{ den, ... }:
{

  den.aspects.fish = {
    includes = [
      (den.batteries.user-shell "fish")
      den.aspects.shell
    ];
    homeManager = {
      programs.fish.enable = true;

    };
    provides.to-host = { pkgs, ... }: {
      os.environment.shells = [ pkgs.fish ];
    };
  };

}
