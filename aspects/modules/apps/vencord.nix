{ den, ... }:
{
  flake-file.inputs.nixcord.url = "github:kaylorben/nixcord";

  den.aspects.vencord = {
    homeManager =
      { ... }:
      {
        programs.nixcord = {
          enable = true;
          vesktop.enable = true;
          dorion.enable = true;
          quickCss = "";
          config = {
            useQuickCss = true;
            frameless = true;
            plugins.ignoreActivities = {
              enable = true;
              ignorePlaying = true;
              ignoreWatching = true;
            };
          };
          dorion = {
            theme = "dark";
            zoom = "1.1";
            blur = "acrylic";
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
        };
      };
  };
}
