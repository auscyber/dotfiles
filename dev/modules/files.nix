{ inputs, ... }:
{
  imports = [ inputs.files.flakeModules.default ];
  perSystem = psArgs: {
    files.gitToplevel = inputs.input-branches;
    make-shells.default.packages = [ psArgs.config.files.writer.drv ];
  };
}
