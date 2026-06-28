{ den, ... }:
{

  den.aspects.fish = {
    includes = [
      (den.batteries.user-shell "fish")
      den.aspects.shell
    ];
    homeManager = {

      stylix.targets.fish.enable = false;
      programs.fish.enable = true;
    };
    provides.to-host = { pkgs, ... }: {
      os.environment.shells = [ pkgs.fish ];
      os.programs.fish.enable = true;
    };
  };

}
