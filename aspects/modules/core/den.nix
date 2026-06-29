{
  inputs,
  den,
  lib,
  __findFile,
  ...
}:

{
  _module.args.__findFile = den.lib.__findFile;
  flake-file.inputs.den.url = "github:denful/den/latest";
  imports = [
    (inputs.flake-file.flakeModules.dendritic or { })
    #    inputs.den.flakeModules.strict
    (inputs.den.flakeModules.dendritic or { })
  ];
  patchedInputs.den = { };
  den.schema.user.classes = lib.mkDefault [ "homeManager" ];

  den.default = {

    nixos.system.stateVersion = "23.11"; # Did you read the comment?
    darwin.system.stateVersion = 5;

    homeManager.home.stateVersion = "24.05";
    includes = [
      den.batteries.inputs'
      den.batteries.self'
      den.batteries.define-user
      den.batteries.hostname
    ];

  };

  # Did you read the comment?

}
