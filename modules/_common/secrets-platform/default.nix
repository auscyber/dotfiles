{
  config,
  lib,
  pkgs,
  hostname,
  ...
}:
let
  cfg = config.auscybernix.secrets;
  inherit (pkgs.stdenv.hostPlatform) isDarwin isLinux;
in
lib.mkIf cfg.enable (
  lib.mkMerge [
    # ── Shared: use the host SSH key for age/sops decryption ─────────────────
    {
      sops.age.sshKeyPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];
    }

    # ── NixOS-only: local rekey storage + configId from hostname ─────────────
    (lib.mkIf isLinux {
      age.rekey.storageMode = "local";
      auscybernix.secrets.configId = "${hostname}";
    })
  ]
)
