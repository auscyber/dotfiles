{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.services.yabai;

  toYabaiConfig =
    opts: concatStringsSep "\n" (mapAttrsToList (p: v: "yabai -m config ${p} ${toString v}") opts);

  configFile =
    mkIf (cfg.config != { } || cfg.extraConfig != "")
      "${pkgs.writeScript "yabairc" (
        (if (cfg.config != { }) then "${toYabaiConfig cfg.config}" else "")
        + optionalString (cfg.extraConfig != "") ("\n" + cfg.extraConfig + "\n")
      )}";
in

{
  options = with types; {
    services.yabai.enable = mkOption {
      type = bool;
      default = false;
      description = "Whether to enable the yabai window manager.";
    };

    services.yabai.package = mkOption {
      type = path;
      default = pkgs.yabai;
      description = "The yabai package to use.";
    };

    services.yabai.enableScriptingAddition = mkOption {
      type = bool;
      default = false;
      description = ''
        Whether to enable yabai's scripting-addition.
        SIP must be disabled for this to work.
      '';
    };
    services.yabai.errorLogFile = lib.mkOption {
      type = with lib.types; nullOr (either path str);
      defaultText = lib.literalExpression "\${config.home.homeDirectory}/Library/Logs/yabai/err.log";
      example = "/Users/khaneliman/Library/Logs/yabai.log";
      description = "Absolute path to log all stderr output.";
    };

    services.yabai.outLogFile = lib.mkOption {
      type = with lib.types; nullOr (either path str);
      defaultText = lib.literalExpression "\${config.home.homeDirectory}/Library/Logs/yabai/out.log";
      example = "/Users/khaneliman/Library/Logs/yabai.log";
      description = "Absolute path to log all stdout output.";
    };

    services.yabai.config = mkOption {
      type = attrs;
      default = { };
      example = literalExpression ''
        {
          focus_follows_mouse = "autoraise";
          mouse_follows_focus = "off";
          window_placement    = "second_child";
          window_opacity      = "off";
          top_padding         = 36;
          bottom_padding      = 10;
          left_padding        = 10;
          right_padding       = 10;
          window_gap          = 10;
        }
      '';
      description = ''
        Key/Value pairs to pass to yabai's 'config' domain, via the configuration file.
      '';
    };

    services.yabai.extraConfig = mkOption {
      type = lines;
      default = "";
      example = literalExpression ''
        yabai -m rule --add app='System Preferences' manage=off
      '';
      description = "Extra arbitrary configuration to append to the configuration file";
    };
  };

  config = lib.mkMerge [
    (mkIf (cfg.enable) {
      assertions = [
        (lib.hm.assertions.assertPlatform "services.yabai" pkgs lib.platforms.darwin)
      ];
      home.packages = [ cfg.package ];

      launchd.agents.yabai = {
        enable = true;
        config = {
          ProcessType = "Interactive";
          ProgramArguments = [
            "${cfg.package}/bin/yabai"
          ]
          ++ optionals (cfg.config != { } || cfg.serviceConfig.extraConfig != "") [
            "-c"
            configFile
          ];
          EnvironmentVariables.PATH = "${cfg.package}/bin:/usr/bin:/bin:/usr/sbin:/sbin";
          KeepAlive = true;
          RunAtLoad = true;
          StandardErrorPath = cfg.errorLogFile;
          StandardOutPath = cfg.outLogFile;

        };

      };
      services.yabai = {
        errorLogFile = lib.mkOptionDefault "${config.home.homeDirectory}/Library/Logs/yabai/yabai.err.log";
        outLogFile = lib.mkOptionDefault "${config.home.homeDirectory}/Library/Logs/yabai/yabai.out.log";
      };
    })

    # TODO: [@cmacrae] Handle removal of yabai scripting additions
    (mkIf (cfg.enableScriptingAddition) {
      launchd.agents.yabai-sa = {
        config.ProgramArguments = [
          "sudo"
          "-E"
          "${cfg.package}/bin/yabai"
          "--load-sa"
        ];
        config.EnvironmentVariables.Path = "${cfg.package}/bin:/usr/bin:/bin:/usr/sbin:/sbin";
        config.RunAtLoad = true;
        config.KeepAlive.SuccessfulExit = false;
      };

    })
  ];
}
