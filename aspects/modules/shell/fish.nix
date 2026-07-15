{ den, inputs, ... }:
{

  den.aspects.fish = { user, host, ... }: {
    includes = [
      (den.batteries.user-shell "fish")
      den.aspects.shell
    ];
    homeManager = {
      disabledModules = [ "${inputs.stylix}/modules/fish/hm.nix" ];

      programs.fish.enable = true;
    };
    os = { pkgs, ... }: {
      environment.shells = [ pkgs.fish ];
      programs.fish.enable = true;
    };
  };

}
