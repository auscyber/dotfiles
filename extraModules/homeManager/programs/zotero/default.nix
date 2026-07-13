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
  # Zotero keeps profiles.ini next to a Profiles/ dir on darwin, and under
  # ~/.zotero/zotero on Linux (not ~/.zotero, which is the vendor dir).
  linuxConfigPath = ".zotero/zotero";
  darwinConfigPath = "Library/Application Support/Zotero";
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
          vendorPath = ".zotero";
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
    # mkFirefoxModule points <profile>/extensions at
    # "${env}/share/mozilla/extensions/{ec8030f7-c20a-464f-9b0e-13a3a9e97384}",
    # the Firefox vendor layout. Zotero has no such dir: it scans
    # <profile>/extensions directly for <addon-id>.xpi, which is where
    # fetchZoteroAddon installs (share/zotero/extensions). Override the link.
    home.file = lib.mkMerge (
      lib.flip lib.mapAttrsToList cfg.profiles (
        _: profile: {
          "${cfg.profilesPath}/${profile.path}/extensions" = lib.mkIf (profile.extensions.packages != [ ]) (
            lib.mkForce {
              source =
                let
                  extensionsEnvPkg = pkgs.buildEnv {
                    name = "hm-zotero-extensions";
                    paths = profile.extensions.packages;
                  };
                in
                "${extensionsEnvPkg}/share/zotero/extensions";
              recursive = true;
              force = true;
            }
          );
        }
      )
    );

    # Zotero itself is not from nixpkgs here (Homebrew cask on darwin), and
    # wrapFirefox is linux-only. Setting package = null makes mkFirefoxModule
    # manage the profile only; the darwinDefaultsId assertion covers this.
    programs.zotero.package = lib.mkDefault (
      if pkgs.stdenv.hostPlatform.isDarwin then
        null
      else
        pkgs.wrapFirefox (pkgs.zotero.overrideAttrs (attrs: {
          passthru = (attrs.passthru or { }) // {
            applicationName = "Zotero";
            binaryName = "zotero";
          };
          pname = "zotero";
          dontFixup = false;
        })) { }
    );
  };

}
