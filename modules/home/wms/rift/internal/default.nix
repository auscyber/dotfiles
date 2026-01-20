{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.services.rift;
  configFile = pkgs.writers.writeTOML "rift-settings" cfg.settings;

in
{

  options.services.rift = {
    enable = lib.mkEnableOption "Enable rift window manager ";
    package = lib.mkPackageOption pkgs "rift" {
    };
    extraPackages = lib.mkOption {
      type = with lib.types; listOf package;
      default = [ ];
      description = "Additional packages to be made available to rift.";
    };
    errorLogFile = lib.mkOption {
      type = with lib.types; nullOr (either lib.types.path lib.types.str);
      default = "${config.home.homeDirectory}/Library/Logs/rift/err.log";
      defaultText = lib.literalExpression "\${config.home.homeDirectory}/Library/Logs/rift/err.log";
      example = "/Users/username/Library/Logs/rift/err.log";
      description = "Absolute path to log all stderr output.";
    };

    outLogFile = lib.mkOption {
      type = with lib.types; nullOr (either lib.types.path lib.types.str);
      default = "${config.home.homeDirectory}/Library/Logs/rift/out.log";
      defaultText = lib.literalExpression "\${config.home.homeDirectory}/Library/Logs/rift/out.log";
      example = "/Users/username/Library/Logs/rift/out.log";
      description = "Absolute path to log all stdout output.";
    };
    settings = lib.mkOption {
      type = lib.types.attrs;
      default = { };
      description = "Settings for rift window manager";

    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ cfg.package ];
    launchd.agents.rift = {
      enable = true;
      config = {
        ProccessType = "Interactive";
        EnvironmentVariables = {
          PATH = "/bin:/sbin:/usr/bin:" + (lib.makeBinPath ([ cfg.package ] ++ cfg.extraPackages));

        };
        ProgramArguments = [
          "${cfg.package}/bin/rift"
        ]
        ++ [
          "--config"
          "${configFile}"
        ];
        RunAtLoad = true;
        KeepAlive = true;
        StandardOutPath = cfg.outLogFile;
        StandardErrorPath = cfg.errorLogFile;

      };
    };
  };

}
