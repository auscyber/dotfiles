{ inputs, ... }:
{
  imports = [ inputs.input-branches.flakeModules.default ];

  perSystem = psArgs: {
    treefmt.projectRoot = inputs.input-branches;
    make-shells.default.packages = psArgs.config.input-branches.commands.all;
  };
}
