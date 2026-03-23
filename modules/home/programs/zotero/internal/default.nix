{
  config,
  pkgs,
  lib,
  inputs,
  ...
}:

let
  cfg = config.programs.zotero;
  modulePath = [
    "programs"
    "zotero"
  ];
  linuxConfigPath = ".zotero";
  darwinConfigPath = "Library/Application Support/Zotero";
  moduleName = lib.concatStringsSep "." modulePath;
  mkFirefoxModule = import "${inputs.home-manager.outPath}/modules/programs/firefox/mkFirefoxModule.nix";
in
{

  imports = [
    (mkFirefoxModule {
      inherit modulePath;
      name = "Zotero";
      wrappedPackageName = "zotero";
      unwrappedPackageName = "zotero";
      platforms = {
        linux = {
          vendorPath = linuxConfigPath;
          configPath = linuxConfigPath;

        };
        darwin = {
          configPath = darwinConfigPath;
          darwinDefaultsId = "org.zotero.zotero";

        };
      };

    })
  ];
  config = lib.mkIf cfg.enable {
    home.file = lib.mkMerge (
      lib.flip lib.mapAttrsToList cfg.profiles (
        _: profile: {
          "${cfg.profilesPath}/${profile.path}/extensions" = lib.mkIf (profile.extensions.packages != [ ]) (
            lib.mkForce {
              source =
                let
                  extensionsEnvPkg = pkgs.symlinkJoin {
                    name = "hm-zotero-extensions";
                    paths = profile.extensions.packages;
                  };
                in
                lib.mkForce "${extensionsEnvPkg}";
              recursive = true;
              force = true;
            }
          );

        }
      )
    );
    programs.zotero = {

      package = pkgs.wrapFirefox (pkgs.zotero.overrideAttrs (attrs: {
        passthru = (attrs.passthru or { }) // {
          applicationName = "Zotero";
          binaryName = "zotero";
        };
        pname = "zotero";
        dontFixup = false;
      })) { };
    };
    #    home.packages = with pkgs; [ zotero ];
  };

}
