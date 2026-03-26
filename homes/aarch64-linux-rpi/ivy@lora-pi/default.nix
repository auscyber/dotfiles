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
    {
      auscybernix.modules.enable = {
        allConfigs        = true;
        nix               = true;
        secrets           = true;
        secrets-platform  = true;
        standalone        = true;
        default           = true;
        gpg               = true;
        neovim            = true;
        fish              = true;
        shell             = true;
        ext-sops          = true;
        ext-agenix        = true;
        ext-agenix-rekey  = true;
      };
      auscybernix.standalone.enable = true;
    }

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
