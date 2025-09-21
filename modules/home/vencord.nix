{
  config,
  pkgs,
  inputs,
  lib,
  ...
}:
let
  cfg = config.auscybernix.programs.vencord;
in
{

  options.auscybernix.programs.vencord = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Whether to install and configure Vencord (Discord client with plugins and themes support).";
    };
  };
  config = lib.mkIf cfg.enable {

    programs.nixcord = {
      enable = true; # Enable Nixcord (It also installs Discord)
      vesktop.enable = true; # Vesktop
      dorion.enable = true; # Dorion
      quickCss = "some CSS"; # quickCSS file
      config = {
        useQuickCss = true; # use out quickCSS

        frameless = true; # Set some Vencord options
        plugins = {
          ignoreActivities = {
            # Enable a plugin and set some options
            enable = true;
            ignorePlaying = true;
            ignoreWatching = true;
            #          ignoredActivities = [ "someActivity" ];
          };
        };
      };
      dorion = {
        theme = "dark";
        zoom = "1.1";
        blur = "acrylic"; # "none", "blur", or "acrylic"
        sysTray = true;
        openOnStartup = true;
        autoClearCache = true;
        disableHardwareAccel = false;
        rpcServer = true;
        rpcProcessScanner = true;
        pushToTalk = true;
        pushToTalkKeys = [ "RControl" ];
        desktopNotifications = true;
        unreadBadge = true;
      };
      extraConfig = {
        # Some extra JSON config here
        # ...
      };
    };
  };
}
