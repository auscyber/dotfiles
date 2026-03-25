{
  config,
  pkgs,
  ...
}:

{

  auscybernix.wms.yabai.scratchpads = {
    "1password" = {
      detectionRules = {
        app = "^1Password$";
      };
      rules = [ "grid=11:11:1:1:9:9" ];
    };

    "discord" = {
      detectionRules = {
        app = "^Discord$";
        "title!" = "^Discord Updater$";
      };
      rules = [ "grid=11:11:1:1:9:9" ];
    };

    "beeper" = {
      detectionRules = {
        app = "^Beeper$";
        "title!" = "^Beeper Updater$";
      };
      rules = [ "grid=11:11:1:1:9:9" ];
    };

  };
  auscybernix.wms.hyprland.scratchpads = {
    beeper = {
      command = "${pkgs.beeper}/bin/beeper";
      windowClass = "beeper";
      extraRules = [
        "size 1600 900"
      ];
    };
    discord = {
      command = "${pkgs.discord}/bin/Discord";
      windowClass = "discord";
      extraRules = [
        "size 1600 900"
      ];
    };

  };
}
