{ den, ... }:
{
  # ── Browsers aspect ───────────────────────────────────────────────────────────
  # Home-manager: Zen browser (privacy-focused Firefox fork), Helium browser.
  # Include in a user aspect; enable individual browsers via option flags.
  den.aspects.browsers = {
    homeManager =
      { config, lib, pkgs, ... }:
      let
        cfgZen = config.auscybernix.browsers.zen-browser;
        cfgHelium = config.auscybernix.browsers.helium;
        stdenv = pkgs.stdenv;
        policies = {
          DisableAppUpdate = true;
          DisableTelemetry = true;
        };
      in
      {
        options.auscybernix.browsers.zen-browser = {
          enable = lib.mkOption {
            type = lib.types.bool;
            default = false;
            description = "Enable Zen Browser.";
          };
          profileName = lib.mkOption {
            type = lib.types.str;
            default = "ivy (Default)";
            description = "Name of the default Zen Browser profile.";
          };
        };
        options.auscybernix.browsers.helium.enable =
          lib.mkEnableOption "Helium browser";

        config = lib.mkMerge [
          # ── Zen browser ──────────────────────────────────────────────────
          (lib.mkIf cfgZen.enable {
            stylix.targets.zen-browser.profileNames = [ cfgZen.profileName ];
            programs.zen-browser = {
              enable = true;
              nativeMessagingHosts = [ pkgs._1password-gui-beta ];
              darwinDefaultsId = "app.zen-browser.zen";
              package = lib.mkForce (
                if stdenv.isDarwin then
                  null
                else
                  pkgs.wrapFirefox
                    (pkgs.zen-browser.override { inherit policies; })
                    {
                      extraPrefs = config.programs.zen-browser.extraPrefs;
                      extraPrefsFiles = config.programs.zen-browser.extraPrefsFiles;
                      nativeMessagingHosts = config.programs.zen-browser.nativeMessagingHosts;
                    }
              );
              inherit policies;
              profiles."${cfgZen.profileName}" = {
                search = {
                  force = true;
                  engines = {
                    kagi = {
                      name = "Kagi Search";
                      urls = [ { template = "https://kagi.com/search"; params = [ { name = "q"; value = "{searchTerms}"; } ]; } ];
                      icon = "${pkgs.nixos-icons}/share/icons/hicolor/scalable/apps/kagi-search.svg";
                      definedAliases = [ "@kagi" ];
                    };
                    nix-packages = {
                      name = "Nix Packages";
                      urls = [ { template = "https://search.nixos.org/packages"; params = [ { name = "type"; value = "packages"; } { name = "query"; value = "{searchTerms}"; } ]; } ];
                      icon = "${pkgs.nixos-icons}/share/icons/hicolor/scalable/apps/nix-snowflake.svg";
                      definedAliases = [ "@np" ];
                    };
                    unduck = {
                      name = "UnDuck";
                      urls = [ { template = "https://unduck.link"; params = [ { name = "q"; value = "{searchTerms}"; } ]; } ];
                    };
                    scholar = {
                      name = "Google Scholar";
                      urls = [ { template = "https://scholar.google.com/scholar"; params = [ { name = "q"; value = "{searchTerms}"; } ]; } ];
                      definedAliases = [ "@sch" ];
                    };
                    "Home-Manager-Options" = {
                      urls = [ { template = "https://home-manager-options.extranix.com"; params = [ { name = "query"; value = "{searchTerms}"; } { name = "release"; value = "master"; } ]; } ];
                      definedAliases = [ "@hm" ];
                    };
                  };
                  default = "kagi";
                  privateDefault = "kagi";
                };
                isDefault = true;
                settings = {
                  "extensions.autoDisableScopes" = 0;
                  "zen.welcome-screen.seen" = true;
                };
                containers = {
                  Queer = { id = 3; color = "pink"; };
                  Uni = { color = "green"; icon = "fruit"; id = 2; };
                  Shopping = { color = "blue"; icon = "cart"; id = 1; };
                };
                extensions.packages = with pkgs.nur.repos.rycee.firefox-addons; [
                  auto-tab-discard
                  libkey-nomad
                  onepassword-password-manager
                  zotero-connector
                  kagi-search
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
                lib.hm.dag.entryAfter [ "writeBoundary" ] ''
                  rm ${profiles-ini}.backup
                  mv ${profiles-ini} ${profiles-ini}.generate
                  cat ${profiles-ini}.generate > ${profiles-ini}
                  echo ZenAvatarPath=chrome://browser/content/zen-avatars/avatar-01.svg >> ${profiles-ini}
                '';
            };
          })

          # ── Helium ────────────────────────────────────────────────────────
          (lib.mkIf cfgHelium.enable {
            home.packages = with pkgs; [ helium ];
            programs.helium = {
              enable = true;
              extensions = [
                { id = "cdglnehniifkbagbbombnjghhcihifij"; }
                { id = "aeblfdkhhhdcdjpifhhbdiojplfjncoa"; } # 1Password
                { id = "ekhagklcjbdpajgpjgmbionohlpdbjgc"; } # Zotero
                { id = "lkoeejijapdihgbegpljiehpnlkadljb"; } # libKey nomad
                { id = "hghakoefmnkhamdhenpbogkeopjlkpoa"; } # Lean library
              ];
            };
          })
        ];
      };
  };
}
