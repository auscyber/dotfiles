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
      # Plain `pkgs`, not `pkgs.extend inputs.nh.overlays.default`. The nh
      # overlay is already declared in `den.default.overlays` above, so it is
      # collected into `_collectedOverlays` and applied when aspects/tooling/
      # overlays.nix builds the perSystem `pkgs` — extending here re-applied an
      # overlay that was already present, rebuilding the whole nixpkgs fixpoint
      # to arrive at the same values.
      #
      # Verified rather than assumed: the overlay defines exactly one attribute
      # (`nh`), whose drvPath is identical in `pkgs` and in the extended set,
      # and all 10 packages `denPackages` returns have identical names and
      # identical drvPaths either way.
      nhPackages = den.lib.nh.denPackages { fromFlake = true; } pkgs;
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
