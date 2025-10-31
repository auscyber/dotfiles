{
  config,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.auscybernix.sudo.pam;

in
{
  options.auscybernix.sudo.pam = {
    enable = lib.mkEnableOption "Enable sudo PAM configuration to allow passwordless sudo";
    touchIdAuth = lib.mkEnableOption "Enable Touch ID authentication for sudo on macOS";
    sshAgentAuth = {
      enable = lib.mkEnableOption "Enable SSH Agent authentication for sudo on macOS";
      authorisedKeys = lib.mkOption {
        type = lib.types.str;
        default = "/etc/authorized_keys/\${user}.keys";
      };
    };

  };

  config = lib.mkIf cfg.enable ({

    security.pam.services.sudo_local.text = lib.concatLines [
      (lib.optional cfg.touchIdAuth "auth       sufficient     pam_tid.so")
      (lib.optional cfg.sshAgentAuth.enable ''
        auth       sufficient     ${pkgs.pam_rssh}/lib/libpam_rssh.dylib auth_key_file=${cfg.sshAgentAuth.authorisedKeys}
      '')

    ];

  });

}
