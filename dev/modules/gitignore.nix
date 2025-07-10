{ lib, config, ... }:
{
  options.gitignore = lib.mkOption {
    type = lib.types.lines;
    apply = lib.flip lib.pipe [
      (lib.splitString "\n")
      lib.naturalSort
      (lib.concatStringsSep "\n")
    ];
  };
  config = {
    gitignore = "result";
    perSystem =
      { pkgs, ... }:
      {
        files.files = [
          {
            path_ = ".gitignore";
            drv = pkgs.writeText ".gitignore" config.gitignore;
          }
        ];
      };
  };
}
