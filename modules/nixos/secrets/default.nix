{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.auscybernix.secrets;
in
{
  config = lib.mkIf cfg.enable {
    sops.age.sshKeyPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];
  };
}
