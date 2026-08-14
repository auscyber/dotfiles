{ lib, ... }: {
  nvfetcher.sources.proton-ge-bin = {
    src.github = "gloriouseggroll/proton-ge-custom";
    fetch.tarball = "https://github.com/GloriousEggroll/proton-ge-custom/releases/download/$ver/$ver-x86_64.tar.gz";
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
                settingsFile = prev.writeText "user_settings.py" (toPySettings settings);
                proton = geProton.steamcompattool;
              in
              prev.runCommand "${geProton.pname}-${geProton.version}-with-settings"
                {
                  outputs = [
                    "out"
                    "steamcompattool"
                  ];
                }
                ''
                  mkdir -p $out
                  cp -r ${proton}/. $out/
                  chmod -R u+w $out
                  cp ${settingsFile} $out/user_settings.py
                  ln -s $out $steamcompattool
                '';
          in
          lib.makeOverridable build;
        proton-ge-bin = prev.proton-ge-bin.overrideAttrs { inherit (sources.proton-ge-bin) src version; };
      };
    };

    nixos =
      {
        config,
        pkgs,
        ...
      }:
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
