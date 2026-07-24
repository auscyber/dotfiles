{ lib, ... }:
{
  nvfetcher.sources.proton-ge-bin = {
    src.github = "gloriouseggroll/proton-ge-custom";
    fetch.tarball = "https://github.com/GloriousEggroll/proton-ge-custom/releases/download/$ver/$ver.tar.gz";
  };

  den.aspects.packages.proton-ge-bin.overlays = { sources, ... }: {
    proton-ge-bin = self: super: {
      wrap-proton-ge-bin =
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
            super.runCommand "${geProton.name}-with-settings" { } ''
              mkdir -p $out
              cp -r ${geProton}/* $out/
              chmod -R u+w $out
              cat >$out/user_settings.py <<'EOF'
              ${toPySettings settings}
              EOF
            '';
        in
        lib.makeOverridable build;

      proton-ge-bin = super.proton-ge-bin.overrideAttrs { inherit (sources.proton-ge-bin) src version; };
    };

    nixos =
      { pkgs, config, ... }:
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
            default = null;
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
          programs.steam.proton-ge.wrappedPackage = wrapGeProton {
            geProton = cfg.package;
            settings = cfg.settings;
          };
          programs.steam.extraCompatPackages = [
            (config.programs.steam.proton-ge.wrappedPackage)
          ];

        };
      };

  };
}
