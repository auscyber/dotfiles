{
  config,
  lib,
  pkgs,
  hostname,
  ...
}:
let
  cfg = config.auscybernix.secrets;
in
{
  config = lib.mkIf cfg.enable {
    age.rekey.storageMode = "local";

    auscybernix.secrets.configId = "${hostname}";
    sops.age.sshKeyPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];
  };
}
