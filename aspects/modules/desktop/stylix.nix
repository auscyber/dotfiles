{
  den,
  inputs,
  lib,
  ...
}:

let
  stylixDefaults = {
    enable = true;
    image = ../../backgrounds/phoebebridgers-2.jpg;
    polarity = "dark";
  };

  # Check if user has homeManager class
  isHMUser =
    ctx:
    (ctx.__entityKind or null) == "user"
    && builtins.elem "homeManager" ((ctx.user or { }).classes or [ ]);

  # Check if user is standalone (no darwin/nixos classes on host)
  isStandalone =
    ctx:
    let
      hostClasses = (ctx.host or { }).classes or [ ];
    in
    !builtins.elem "darwin" hostClasses && !builtins.elem "nixos" hostClasses;
in
{

  flake-file.inputs.stylix.url = "github:nix-community/stylix";

  # Policy: inject stylix HM module for standalone users
  den.policies.stylix-standalone-hm =
    ctx:
    if isHMUser ctx && isStandalone ctx then
      [
        (den.lib.policy.provide {
          class = "homeManager";
          module = {
            key = "den:stylix-standalone-hm";
            imports = lib.optional (inputs ? stylix) inputs.stylix.homeModules.default;
            stylix = stylixDefaults;
          };
        })
      ]
    else
      [ ];

  den.aspects.stylix = {

    darwin = {
      imports = lib.optional (inputs ? stylix) inputs.stylix.darwinModules.default;
    };

    nixos = {
      imports = lib.optional (inputs ? stylix) inputs.stylix.nixosModules.default;
    };

    os.stylix = stylixDefaults;

  };

  # Register the policy on user schema
  den.schema.user.includes = [
    den.policies.stylix-standalone-hm
  ];
