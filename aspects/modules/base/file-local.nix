{
  den,
  lib,
  rootPath,
  ...
}:
{
  den.aspects.file-local = {
    homeManager =
      { config, user, ... }:
      {
        config.lib.file.getLocalPath =
          path:
          let
            flakeFolder = user.flakeFolder or "${config.home.homeDirectory}/dotfiles";
          in
          flakeFolder + (lib.strings.removePrefix (builtins.toString rootPath) (builtins.toString path));

        config.lib.file.linkLocalPath =
          path: config.lib.file.mkOutOfStoreSymlink (config.lib.file.getLocalPath path);
      };
  };
}
