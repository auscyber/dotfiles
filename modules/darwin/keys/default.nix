{
  config,
  pkgs,
  lib,
  ...
}:
{
  auscybernix.user.username = config.system.primaryUser;
}
