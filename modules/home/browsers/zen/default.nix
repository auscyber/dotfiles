{
  config,
  pkgs,
  inputs,
  lib,
  ...
}:
let
  cfg = config.auscybernix.browsers.zen-browser;
  stdenv = pkgs.stdenv;
  policies = {
    DisableAppUpdate = true;
    DisableTelemetry = true;
    # find more options here: https://mozilla.github.io/policy-templates/
  };
in
{
  options.auscybernix.browsers.zen-browser = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Enable Zen Browser (a privacy-focused browser based on Firefox).";
    };
    profileName = lib.mkOption {
      type = lib.types.str;
      default = "ivy (Default)";
      description = "The name of the default profile to use.";
    };

  };
  config = lib.mkIf cfg.enable {
    stylix.targets.zen-browser.profileNames = [ cfg.profileName ];
    programs.zen-browser = {
      enable = true;
      nativeMessagingHosts = [ pkgs._1password-gui-beta ];

      darwinDefaultsId = "app.zen-browser.zen";
      package = lib.mkForce (
        if stdenv.isDarwin then
          null
        else
          pkgs.wrapFirefox
            (pkgs.zen-browser.override {
              inherit policies;
            })
            {
              extraPrefs = config.programs.zen-browser.extraPrefs;
              extraPrefsFiles = config.programs.zen-browser.extraPrefsFiles;
              nativeMessagingHosts = config.programs.zen-browser.nativeMessagingHosts;
            }
      );
      inherit policies;
      profiles."${cfg.profileName}" = {
        search.default = "https://unduck.link?q=%s";
        isDefault = true;
        settings = {
          "extensions.autoDisableScopes" = 0;
          "zen.welcome-screen.seen" = true;
        };
        containers = {
          Queer = {
            id = 3;
            color = "pink";
          };
          Uni = {
            color = "green";
            icon = "fruit";
            id = 2;
          };
          Shopping = {
            color = "blue";
            icon = "cart";
            id = 1;
          };
        };
        extensions.packages = with pkgs.nur.repos.rycee.firefox-addons; [
          auto-tab-discard
          libkey-nomad
          onepassword-password-manager
          zotero-connector
          ublock-origin
        ];

      };
    };

    home.activation = lib.optionalAttrs stdenv.hostPlatform.isDarwin {
      zen-browser =
        let

          profiles-ini =
            if stdenv.hostPlatform.isLinux then
              "${config.xdg.configHome}/zen/profiles.ini"
            else
              "${config.home.homeDirectory}/Library/\"Application Support\"/zen/profiles.ini";
        in
        inputs.home-manager.lib.hm.dag.entryAfter [ "writeBoundary" ] ''
          			rm ${profiles-ini}.backup
          			mv ${profiles-ini} ${profiles-ini}.generate
          			cat ${profiles-ini}.generate > ${profiles-ini}
          		echo ZenAvatarPath=chrome://browser/content/zen-avatars/avatar-01.svg >> ${profiles-ini}


          	'';
    };
  };
}
