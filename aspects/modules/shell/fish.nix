{ den, inputs, ... }:
{

  den.aspects.fish = { user, ... }: {
    includes = [
      (den.batteries.user-shell "fish")
      den.aspects.shell
    ];
    homeManager = {
      disabledModules = [ "${inputs.stylix}/modules/fish/hm.nix" ];

      programs.fish.enable = true;
    };
    provides.to-host = { pkgs, host, ... }: {
      os.environment.shells = [ pkgs.fish ];
      os.programs.fish.enable = true;
    };
  };

}
