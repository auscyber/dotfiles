{
  den,
  inputs,
  ...
}:
{
  den.aspects.fish =

    {
      includes = [
        (den.batteries.user-shell "fish")
        den.aspects.shell
      ];
      homeManager = {
        disabledModules = [ "${inputs.stylix}/modules/fish/hm.nix" ];

        programs.fish.enable = true;
      };
      provides.to-hosts.os = { pkgs, ... }: {
        environment.shells = [ pkgs.fish ];
        programs.fish.enable = true;
      };
    };
}
