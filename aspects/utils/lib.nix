{
  den,
  lib,
  rootPath,
  ...
}:
{
  flake.lib.relativeToFlake =
    path: lib.strings.removePrefix (toString rootPath) (toString path);

  den.aspects.lib.homeManager =
    { config, ... }:
    {
      lib.file.getLocalPath =
        path:
        config.flakeFolder + lib.strings.removePrefix (toString rootPath) (toString path);

      lib.file.linkLocalPath =
        path: config.lib.file.mkOutOfStoreSymlink (config.lib.file.getLocalPath path);
    };

  den.default.includes = [ den.aspects.lib ];
}
