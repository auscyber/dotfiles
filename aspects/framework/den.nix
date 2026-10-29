{
  inputs,
  den,
  lib,
  __findFile,
  ...
}:
{
  _module.args.__findFile = den.lib.__findFile;
  # Patched through ../../patched-inputs.nix, which flake.nix reads before any
  # evaluation, so a den patch only bootstraps if the tree still evaluates
  # without it: `write-patched-inputs` runs on the unpatched den the first time.
  # The other patches/den/*.patch stay dormant.
  ff.den.url = "github:denful/den/main";
  ff.den.patch = {
    enable = true;
    autoIncludePatches = false;
    # `den.lib.gen`: the roster den itself evaluates with.
    patches = [ ../../patches/den/lib-gen.patch ];
  };
  imports = [
    (inputs.flake-file.flakeModules.dendritic or { })
    #    inputs.den.flakeModules.strict
    (inputs.den.flakeModules.dendritic or { })
  ];
  den.schema.user.classes = lib.mkDefault [ "homeManager" ];

  den.default = {
    # `mkDefault`, so a host can state its own. These are fleet-wide floors for
    # machines that predate caring; a box installed later says so itself
    # (../hosts/celler2.nix is "25.11") and without the lower priority that is
    # a flat conflict, which only shows when something forces the whole
    # config -- `age.secrets` does not, so it surfaced as a failed BUILD rather
    # than a failed eval.
    nixos.system.stateVersion = lib.mkDefault "23.11"; # Did you read the comment?
    darwin.system.stateVersion = lib.mkDefault 5;

    homeManager.home.stateVersion = lib.mkDefault "24.05";
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
