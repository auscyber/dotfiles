{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.services.sccache;
  sccacheBin = "${cfg.package}/bin/sccache";
in
{
  options.services.sccache = {
    enable = lib.mkEnableOption "sccache compiler cache";

    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.sccache;
    };

    cacheDir = lib.mkOption {
      type = lib.types.str;
      default = "${config.xdg.cacheHome}/sccache";
    };

    cacheSize = lib.mkOption {
      type = lib.types.str;
      default = "50G";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ cfg.package ];

    home.sessionVariables = {
      RUSTC_WRAPPER = sccacheBin;
      SCCACHE_DIR = cfg.cacheDir;
      SCCACHE_CACHE_SIZE = cfg.cacheSize;
    };

    home.activation.createSccacheDir = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      mkdir -p ${cfg.cacheDir}
    '';

    # -------------------------
    # Linux (systemd user)
    # -------------------------
    systemd.user.services.sccache = lib.mkIf pkgs.stdenv.isLinux {
      Unit = {
        Description = "sccache compiler cache daemon";
        After = [ "default.target" ];
      };

      Service = {
        ExecStart = "${sccacheBin} --start-server";
        Restart = "always";
        Environment = [
          "SCCACHE_DIR=${cfg.cacheDir}"
          "SCCACHE_CACHE_SIZE=${cfg.cacheSize}"
        ];
      };

      Install = {
        WantedBy = [ "default.target" ];
      };
    };

    systemd.user.startServices = lib.mkIf pkgs.stdenv.isLinux "sd-switch";

    # -------------------------
    # macOS (launchd)
    # -------------------------
    launchd.agents.sccache = lib.mkIf pkgs.stdenv.isDarwin {
      enable = true;

      config = {
        ProgramArguments = [
          sccacheBin
          "--start-server"
        ];

        RunAtLoad = true;
        KeepAlive = true;

        EnvironmentVariables = {
          SCCACHE_DIR = cfg.cacheDir;
          SCCACHE_CACHE_SIZE = cfg.cacheSize;
        };
      };
    };
  };
}
