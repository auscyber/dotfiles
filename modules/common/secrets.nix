{
  config,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.auscybernix.secrets;
in
{
  options = {
    auscybernix.secrets = {
      enable = lib.mkEnableOption "Enable sops integration for managing secrets.";
    };
  };
  config = lib.mkIf cfg.enable {
    sops.defaultSopsFile = ../../secrets/default.yaml;
    #  sops.age.sshKeyPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];
    sops.secrets.github_token = { };
    sops.secrets.password = { };

  };
}
