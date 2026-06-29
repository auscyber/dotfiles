{
  den,
  inputs,
  lib,
  ...
}:

let
  stylixDefaults = {
    enable = true;
    image = ../../../backgrounds/phoebebridgers-2.jpg;
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
    if (ctx ? host) && ctx.host ? class then
      ctx.host.class != "darwin" && ctx.host.class != "nixos"
    else
      true;
in
{

  flake-file.inputs.stylix.url = "github:nix-community/stylix";

  # Policy: inject stylix HM module for standalone users
  den.policies.stylix-standalone-hm =
    ctx@{ user, host }:
    if isHMUser ctx && isStandalone ctx then
      [
        builtins.break
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
  den.schema.default.includes = [
    den.policies.stylix-standalone-hm
  ];
}
