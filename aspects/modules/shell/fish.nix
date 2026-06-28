{ den, inputs, ... }:
{

  den.aspects.fish = {
    includes = [
      (den.batteries.user-shell "fish")
      den.aspects.shell
    ];
    homeManager = {
      disabledModules = [ "${inputs.stylix}/modules/fish/hm.nix" ];

      programs.fish.enable = true;
    };
    provides.to-host = { pkgs, ... }: {
      os.environment.shells = [ pkgs.fish ];
      os.programs.fish.enable = true;
    };
  };

}
