{
  config,
  pkgs,
  lib,
  inputs,
  ...
}:
let
  secretConfig = config.auscybernix.secrets;
in
{
  config = lib.mkMerge [

    (lib.mkIf secretConfig {
      sops.age.sshKeyPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];
    })
    {

      programs.home-manager.enable = true;

      programs.gpg.enable = true;

      auscybernix = {
        shell = {
          enable = true;
          fish = {
            enable = true;
          };
        };
      };
      programs.neovim.enable = true;
      home.stateVersion = "25.05";
    }
  ];

}
