{
  den,
  inputs,
  lib,
  ...
}:
let
  inherit (den.lib.policy) route;
in
{
  ff.devshell.url = "github:numtide/devshell";
  imports = lib.optional (inputs ? "devshell") inputs.devshell.flakeModule;
  den.classes.devshell = { };
  den.policies.devshell-to-flake-parts = _: [
    (route {
      fromClass = "devshell";
      intoClass = "flake-parts";
      path = [
        "devshells"
        "default"
      ];
      adaptArgs = { config, ... }: config.allModuleArgs;
    })
  ];
  den.schema.flake-parts.includes = [ den.policies.devshell-to-flake-parts ];

}
