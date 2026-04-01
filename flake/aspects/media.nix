{ den, ... }:
{
  # ── Media aspect ─────────────────────────────────────────────────────────────
  # Home-manager: Mopidy music server, Vencord (Discord client mod).
  # Include in a user aspect to enable media/communication tools.
  den.aspects.media = {
    homeManager =
      { config, lib, pkgs, ... }:
      {
        options.auscybernix.services.mopidy.enable =
          lib.mkEnableOption "Mopidy music server";
        options.auscybernix.programs.vencord.enable = lib.mkOption {
          type = lib.types.bool;
          default = false;
          description = "Enable Vencord (Discord with plugins and themes).";
        };

        config = lib.mkMerge [
          # ── Mopidy ───────────────────────────────────────────────────────
          (lib.mkIf config.auscybernix.services.mopidy.enable {
            # Mopidy configuration is currently commented out upstream pending
            # a secrets-enabled listenbrainz token.  The option is declared so
            # hosts can set it when the service is ready.
          })

          # ── Vencord ───────────────────────────────────────────────────────
          (lib.mkIf config.auscybernix.programs.vencord.enable {
            programs.nixcord = {
              enable = true;
              vesktop.enable = true;
              dorion.enable = true;
              quickCss = "some CSS";
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
          })
        ];
      };
  };
}
