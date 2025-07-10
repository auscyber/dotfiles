{ inputs, ... }:
{
  imports = [ inputs.input-branches.flakeModules.default ];

  perSystem = psArgs: {
    treefmt.projectRoot = inputs.input-branches;
  };
}
