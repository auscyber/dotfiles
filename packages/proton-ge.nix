{ lib, ... }:
{
  nvfetcher.sources.proton-ge-bin = {
    src.github = "gloriouseggroll/proton-ge-custom";
    fetch.tarball = "https://github.com/GloriousEggroll/proton-ge-custom/releases/download/$ver/$ver.tar.gz";
  };

  den.aspects.packages.proton-ge-bin = {

    overlays = { sources, ... }: {
      proton-ge-bin = final: prev: {
        wrapProtonGE =

          let
            toPySettings =
              attrs:
              "user_settings = {\n"
              + lib.concatStringsSep "\n" (
                lib.mapAttrsToList (k: v: "    ${builtins.toJSON k}: ${builtins.toJSON v},") attrs
              )
              + "\n}\n";

            build =
              {
                geProton,
                settings ? { },
              }:
              let
                settingsDrv = prev.linkFarm "${geProton.name}-user-settings" [
                  {
                    name = "user_settings.py";
                    path = prev.writeText "user_settings.py" (toPySettings settings);
                  }
                ];
              in
              prev.symlinkJoin {
                name = "${geProton.name}-with-settings";
                paths = [
                  settingsDrv
                  geProton
                ];
                ignoreCollisions = true;
              };
          in
          lib.makeOverridable build;
        proton-ge-bin = prev.proton-ge-bin.overrideAttrs { inherit (sources.proton-ge-bin) src version; };
      };
    };

    nixos =
      { config, pkgs, ... }:
      let
        cfg = config.programs.steam.proton-ge;
      in
      {
        options.programs.steam.proton-ge = {
          enable = lib.mkOption {
            type = lib.types.bool;
            default = false;
            description = "Enable Proton GE for Steam.";
          };
          package = lib.mkOption {
            type = lib.types.package;
            default = pkgs.proton-ge-bin;
            description = "Override the Proton GE package used by Steam.";
          };
          settings = lib.mkOption {
            type = lib.types.attrsOf lib.types.str;
            default = { };
            description = "Override the Proton GE settings.";
          };
          wrappedPackage = lib.mkOption {
            type = lib.types.package;
            default = null;
            internal = true;
            description = "Override the wrapped Proton GE package used by Steam.";
          };

        };
        config = lib.mkIf config.programs.steam.proton-ge.enable {
          programs.steam.proton-ge.wrappedPackage = pkgs.wrapProtonGE {
            geProton = cfg.package;
            settings = cfg.settings;
          };
          programs.steam.extraCompatPackages = [
            cfg.wrappedPackage
          ];

        };
      };

  };
}
