{ lib, ... }:
let
  inherit (lib) types mkOption;
in
{
  config.mobile.modules.apps = {
    options.apps = mkOption {
      default = { };
      type = types.attrsOf (
        types.submodule (
          { name, config, ... }:
          {
            options = {
              label = mkOption {
                type = types.str;
                default = name;
              };
              bundleId = mkOption {
                type = types.strMatching "[A-Za-z0-9.-]+";
                description = "How activation tells, over devicectl, whether it is installed.";
              };
              # Exactly one of the two: an App Store app is opened at its
              # store page, a self-signed bundle is pushed with devicectl.
              from = mkOption {
                type = types.attrTag {
                  appStore = mkOption { type = types.ints.positive; };
                  signed = mkOption { type = types.either types.path types.package; };
                };
              };
              setupUrls = mkOption {
                type = types.listOf types.str;
                default = [ ];
                description = "Opened once per generation after install, e.g. `obsidian://…`.";
              };
              storeUrl = mkOption {
                type = types.nullOr types.str;
                readOnly = true;
                default =
                  if config.from ? appStore then
                    "itms-apps://apps.apple.com/app/id${toString config.from.appStore}"
                  else
                    null;
              };
            };
          }
        )
      );
    };
  };
}
