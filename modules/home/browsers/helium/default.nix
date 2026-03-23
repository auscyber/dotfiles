{
  pkgs,
  options,
  config,
  lib,
  ...
}:
let
  inherit (lib) mkOption;

  cfg = config.auscybernix.browsers.helium;
in
{
  options.auscybernix.browsers.helium = {
    enable = lib.mkEnableOption "Helium browser";
  };
  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      helium
    ];
    programs.helium = {
      enable = true;
      extensions = [
        {
          id = "cdglnehniifkbagbbombnjghhcihifij";
        }
        {
          id = "aeblfdkhhhdcdjpifhhbdiojplfjncoa"; # 1Password

        }
        {
          id = "ekhagklcjbdpajgpjgmbionohlpdbjgc"; # Zotero
        }
        {
          id = "lkoeejijapdihgbegpljiehpnlkadljb"; # libKey nomad
        }
        {
          id = "hghakoefmnkhamdhenpbogkeopjlkpoa"; # Lean library
        }
      ];
    };
  };

}
