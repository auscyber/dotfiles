{
  inputs,
  den,
  lib,
  __findFile,
  ...
}:

{
  _module.args.__findFile = den.lib.__findFile;
  flake-file.inputs.den.url = "github:denful/den";
  imports = [
    (inputs.flake-file.flakeModules.dendritic or { })
    #    inputs.den.flakeModules.strict
    (inputs.den.flakeModules.dendritic or { })
  ];
  den.schema.user.classes = lib.mkDefault [ "homeManager" ];

  den.default = {

    nixos.system.stateVersion = "23.11"; # Did you read the comment?
    darwin.system.stateVersion = 5;

    homeManager = {
      home.stateVersion = "24.05"; # Did you read the comment?
    };

    includes = [
      den.batteries.inputs'
      den.batteries.self'
      den.batteries.define-user
      den.batteries.hostname
    ];
  };
  perSystem = { pkgs, ... }: {
    devShells.default = den.lib.nh.denShell {
      fromFlake = true;
      outPrefix = [ "flake" ];
    } pkgs;
  };

}
