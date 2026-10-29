# Activation is pull-based. The Mac serves the generation on the tailnet; a
# fixed bootstrap shortcut on the device fetches `manifest.json` and applies
# it. The bootstrap only changes when its shape does, so it is imported once,
# not per generation.
{ lib, ... }:
let
  inherit (lib) types mkOption;
in
{
  config.mobile.modules.activation =
    { config, name, ... }:
    let
      cfg = config.activation;
      ref = id: key: { ref = { inherit id key; }; };
      state = "dendritic/${name}/generation.txt";
    in
    {
      options.wallpaper = mkOption {
        default = null;
        type = types.nullOr (
          types.submodule {
            options.image = mkOption { type = types.path; };
            options.where = mkOption {
              type = types.enum [
                "Lock Screen"
                "Home Screen"
                "Lock Screen and Home Screen"
              ];
              default = "Lock Screen and Home Screen";
            };
          }
        );
      };

      options.activation = {
        shortcut = mkOption {
          type = types.str;
          default = "Activate ${name}";
          description = "The bootstrap's name. Shortcuts sync to every device, so it names the device.";
        };
        path = mkOption {
          type = types.strMatching "/[A-Za-z0-9/_-]+";
          default = "/mobile/${name}";
          description = "Where the generation is served on the activating Mac's tailnet name.";
        };
        open = mkOption {
          type = types.listOf types.str;
          default = [ ];
          description = "URLs opened on the device each new generation.";
        };
      };

      options.build.manifest = mkOption {
        type = types.attrsOf types.anything;
        readOnly = true;
      };

      config.build.manifest = {
        inherit name;
        inherit (cfg) shortcut;
        # The profile first: it is the only step that needs a decision.
        open =
          lib.optional (config.build.profile != null) "@BASE@/profile.mobileconfig"
          ++ lib.concatMap (a: a.setupUrls) (lib.attrValues config.apps)
          ++ cfg.open;
        apps = lib.mapAttrsToList (_: a: {
          inherit (a) label bundleId storeUrl;
        }) (lib.filterAttrs (_: a: a.from ? appStore) config.apps);
      };

      config.shortcuts.${cfg.shortcut}.actions = [
        { comment = "dendritic bootstrap for ${name}: fetches the generation the Mac is serving."; }
        {
          deviceDetails = {
            id = "model";
            detail = "Device Model";
          };
        }
        {
          ifEquals = {
            input = ref "model" null;
            value = config.device.model;
            "else" = [
              {
                alert = {
                  title = cfg.shortcut;
                  message = "This activates ${config.device.description}, an ${config.device.model}.";
                };
              }
              { stop = { }; }
            ];
          };
        }
        {
          download = {
            id = "fetched";
            url = "@BASE@/manifest.json";
          };
        }
        {
          dictionary = {
            id = "manifest";
            input = ref "fetched" null;
          };
        }
        {
          getFile = {
            id = "applied";
            path = state;
          };
        }
        {
          ifEquals = {
            input = ref "applied" null;
            value = [ (ref "manifest" "generation") ];
            "then" = [
              {
                alert = {
                  title = "Up to date";
                  message = [
                    "Generation "
                    (ref "manifest" "generation")
                    " is already applied. Apply it again?"
                  ];
                  cancel = true;
                };
              }
            ];
          };
        }
        {
          forEach = {
            over = ref "manifest" "open";
            body = [ { openUrl = [ { repeatItem = { }; } ]; } ];
          };
        }
      ]
      ++ lib.optionals (config.wallpaper != null) [
        {
          download = {
            id = "wallpaperImage";
            url = "@BASE@/wallpaper";
          };
        }
        {
          setWallpaper = {
            image = ref "wallpaperImage" null;
            inherit (config.wallpaper) where;
          };
        }
      ]
      ++ [
        {
          text = {
            id = "generation";
            text = [ (ref "manifest" "generation") ];
          };
        }
        {
          saveFile = {
            input = ref "generation" null;
            path = state;
          };
        }
        {
          notify = {
            title = cfg.shortcut;
            body = [
              "Applied generation "
              (ref "manifest" "generation")
            ];
          };
        }
      ];
    };
}
