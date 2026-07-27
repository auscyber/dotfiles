{
  den,
  lib,
  inputs,
  ...
}:
let
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
  ff.nh.url = "github:nix-community/nh";
  patchedInputs.nh = {
    patches = [ ../../patches/nh/edit.patch ];
  };

  den.default = {
    overlays = {
      nh = lib.optional (inputs ? nh) inputs.nh.overlays.default;
    };
  };

  perSystem =
    { pkgs, ... }:
    let
      # Bound once, not inlined at both use sites. `pkgs.extend` rebuilds the
      # whole nixpkgs fixpoint, so writing it out twice did that work (and the
      # `denPackages` call on top of it) twice per system for one identical
      # value.
      nhPackages = den.lib.nh.denPackages { fromFlake = true; } (
        pkgs.extend inputs.nh.overlays.default
      );
    in
    {
      packages = nhPackages;

      devshells.default.packages = builtins.attrValues nhPackages;
    };

  # Policy: set nh environment variables based on user's flakeFolder
  den.policies.nh-env =
    ctx:
    let
      user = ctx.user or { };
      flakeFolder = user.flakeFolder or null;
    in
    if isHMUser ctx && flakeFolder != null then
      [
        (den.lib.policy.provide {
          class = "homeManager";
          module = {
            key = "den:nh-env";
            home.sessionVariables = {
              FLAKE = flakeFolder;
            }
            // lib.optionalAttrs (isStandalone ctx) {
              NH_HOME_FLAKE = flakeFolder;
            };
            home.shellAliases = lib.optionalAttrs (isStandalone ctx) {
              re = "nh home switch";
            };
          };
        })
      ]
    else
      [ ];

  den.schema.user.includes = [
    den.policies.nh-env
  ];
}
