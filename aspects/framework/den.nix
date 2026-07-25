{
  inputs,
  den,
  lib,
  __findFile,
  ...
}:
{
  _module.args.__findFile = den.lib.__findFile;
  # den is NOT patched in-tree: it provides the flake's own evaluation machinery,
  # so producing `den-patched` (via newInputs) requires a full raw-den evaluation
  # first — a bootstrap cycle for any change the flake's eval depends on. The
  # standalone-home guard + scope-identity fixes therefore live config-level (see
  # aspects/base/home-standalone.nix). patches/den/*.patch document the intended
  # upstream changes but stay dormant (no patch.enable).
  ff.den.url = "github:denful/den/latest";
  imports = [
    (inputs.flake-file.flakeModules.dendritic or { })
    #    inputs.den.flakeModules.strict
    (inputs.den.flakeModules.dendritic or { })
  ];
  den.schema.user.classes = lib.mkDefault [ "homeManager" ];

  den.default = {
    nixos.system.stateVersion = "23.11"; # Did you read the comment?
    darwin.system.stateVersion = 5;

    homeManager.home.stateVersion = "24.05";
    #    hmDarwin.targets.darwin.copyApps = true;
    includes = [
      den.batteries.inputs'
      den.batteries.self'
      # define-user provides home.username/homeDirectory: userContext ({host,user})
      # for host-managed users, hmContext ({home}) for standalone homes. Must be a
      # default (not host-only) so the home-scope path fires for standalone homes.
      den.batteries.define-user
    ];
  };
  den.schema.host.includes = [
    den.batteries.hostname
  ];

  # Did you read the comment?
}
