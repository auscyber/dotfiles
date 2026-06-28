{
  inputs,
  lib,
  den,
  ...
}:
let
  correct-inputs = inputs ? nur && inputs ? zen-browser;
  ifTest = b: v: if b then v else { };
in
{
  flake-file = {
    inputs = {
      zen-browser.url = "github:0xc000022070/zen-browser-flake";
      zen-browser.inputs.nixpkgs.follows = "nixpkgs";
      zen-browser.inputs.home-manager.follows = "home-manager";
      nur = {
        url = "github:nix-community/NUR";
        inputs.nixpkgs.follows = "nixpkgs";
      };

    };
  };

  den.aspects.browsers.zen = {

    includes = [
      den.aspects.stylix
      (den.batteries.unfree [ "libkey-nomad" ])
    ];
    provides.to-hosts = { host, ... }: {
      nixos.environment.etc."1password/custom_allowed_browsers" = {
        text = lib.mkAfter ''
          .helium-wrapped
                  helium
                  helium-browser
        '';
        mode = "0755";

      };
    };

    overlays = {
      zen =
        self: super:
        let
          zen-browser = {
            aarch64-darwin = inputs.my-nur.packages.aarch64-darwin.zen-browser;
            x86_64-linux = inputs.zen-browser.packages.x86_64-linux.beta-unwrapped;
            aarch64-linux = inputs.zen-browser.packages.aarch64-linux.beta-unwrapped;
          };
        in

        {
          zen-browser = zen-browser.${self.stdenv.hostPlatform.system} or super.zen-browser;

        };
      nur = inputs.nur.overlays.default;

      firefox = self: super: {
        firefox-addons = super.nur.repos.rycee.firefox-addons;
      };
    };
    homeManager =
      {
        config,
        pkgs,
        inputs',
        lib,
        ...
      }:
      let
        stdenv = pkgs.stdenv;
        policies = {
          DisableAppUpdate = true;
          DisableTelemetry = true;
          # find more options here: https://mozilla.github.io/policy-templates/
        };
        profileName = config.zen.profileName;
      in

      ifTest correct-inputs {

        imports = [ inputs.zen-browser.homeModules.default ];
        options.zen.profileName = lib.mkOption {
          type = lib.types.str;
          default = "ivy (Default)";
        };
        config = {
          home.sessionVariables.BROWSER = "zen";
          stylix.targets.zen-browser.profileNames = [ profileName ];
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
            profiles."${profileName}" = {
              search = {
                force = true;
                engines = {
                  kagi = {
                    name = "Kagi Search";
                    urls = [
                      {
                        template = "https://kagi.com/search";
                        params = [
                          {
                            name = "q";
                            value = "{searchTerms}";
                          }
                        ];
                      }
                    ];

                    icon = "${pkgs.nixos-icons}/share/icons/hicolor/scalable/apps/kagi-search.svg";
                    definedAliases = [ "@kagi" ];
                  };
                  nix-packages = {
                    name = "Nix Packages";
                    urls = [
                      {
                        template = "https://search.nixos.org/packages";
                        params = [
                          {
                            name = "type";
                            value = "packages";
                          }
                          {
                            name = "query";
                            value = "{searchTerms}";
                          }
                        ];
                      }
                    ];

                    icon = "${pkgs.nixos-icons}/share/icons/hicolor/scalable/apps/nix-snowflake.svg";
                    definedAliases = [ "@np" ];
                  };
                  unduck = {
                    name = "UnDuck";
                    urls = [
                      {
                        template = "https://unduck.link";
                        params = [
                          {
                            name = "q";
                            value = "{searchTerms}";
                          }
                        ];
                      }
                    ];
                  };
                  scholar = {
                    name = "Google Scholar";
                    urls = [
                      {
                        template = "https://scholar.google.com/scholar";
                        params = [
                          {
                            name = "q";
                            value = "{searchTerms}";
                          }
                        ];
                      }
                    ];
                    definedAliases = [ "@sch" ];
                  };
                  "Home-Manager-Options" = {
                    urls = [
                      {
                        template = "https://home-manager-options.extranix.com";
                        params = [
                          {
                            name = "query";
                            value = "{searchTerms}";
                          }
                          {
                            name = "release";
                            value = "master";
                          }
                        ];
                      }
                    ];
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
                zotero-connector
                kagi-search
                ublock-origin
              ];

            };
          };

          #home.activation = lib.optionalAttrs stdenv.hostPlatform.isDarwin {
          #  zen-browser =
          #    let

          #      profiles-ini =
          #        if stdenv.hostPlatform.isLinux then
          #          "${config.xdg.configHome}/zen/profiles.ini"
          #        else
          #          "${config.home.homeDirectory}/Library/\"Application Support\"/zen/profiles.ini";
          #    in
          #    inputs.home-manager.lib.hm.dag.entryAfter [ "writeBoundary" ] ''
          #      			rm ${profiles-ini}.backup
          #      			mv ${profiles-ini} ${profiles-ini}.generate
          #      			cat ${profiles-ini}.generate > ${profiles-ini}
          #      		echo ZenAvatarPath=chrome://browser/content/zen-avatars/avatar-01.svg >> ${profiles-ini}

          #      	'';
          #          };
        };
      };

  };
}
