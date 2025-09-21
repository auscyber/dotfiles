{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.auscybernix.keybinds.kanata;
  inherit (pkgs.stdenv.hostPlatform) isDarwin isLinux;
in
{
  options.auscybernix.keybinds.kanata = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Enable kanata service";
    };
    extraPackages = lib.mkOption {
      type = lib.types.listOf lib.types.package;
      default = [ ];
      description = "Extra packages to install when kanata is enabled";
    };
    kanataPort = lib.mkOption {
      type = lib.types.int;
      default = 5829;
    };
    appBundleIds = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
    };
    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.kanata-with-cmd;
      description = "kanata package to use";
    };
    config = lib.mkOption {
      type = lib.types.str;
      default = "${pkgs.kanata}/share/kanata/kanata.kbd";
      description = "kanata config file content";
    };
  };
  config = lib.mkIf cfg.enable (
    lib.mkMerge [
      (lib.mkIf isLinux {
        systemd.user.services.kanata = {
          Unit = {
            Description = "Kanata keyboard remapper";
            After = [ "graphical-session.target" ];

          };
          Service = {
            ExecStart = "${cfg.package}/bin/kanata -c '${cfg.config}'";
            Restart = "on-failure";
            RestartSec = 5;
            Environment = "PATH=${lib.makeBinPath (with pkgs; [ cfg.package ] ++ cfg.extraPackages)}";
          };
        };
        home.packages = with pkgs; [ cfg.package ] ++ cfg.extraPackages;
      })
      (lib.mkIf isDarwin {
        # enable karabiner driver
        launchd.agents.kanata-vk-agent = {
          enable = true;
          config = {
            Label = "org.nixos.kanata-vk-agent";
            ProgramArguments = [
              "${pkgs.kanata-vk-agent}/bin/kanata-vk-agent"
              "-p"
              "${builtins.toString cfg.kanataPort}"
              "-b"
              "${builtins.concatStringsSep "," cfg.appBundleIds}"
            ];
            RunAtLoad = true;
            KeepAlive = {
              Crashed = true;
              SuccessfulExit = false;
            };
            StandardErrorPath = "/tmp/kanata-vk-agent.err";
            StandardOutPath = "/tmp/kanata-vk-agent.out";

          };

        };

        launchd.agents.kanata = {
          enable = true;
          config = {
            ProgramArguments = [
              "/usr/bin/sudo"
              "-E"
              "${cfg.package}/bin/kanata"
              "-p"
              "${builtins.toString cfg.kanataPort}"
              "-c"
              cfg.config
            ];
            StandardErrorPath = "/tmp/kanata.err";
            StandardOutPath = "/tmp/kanata.out";
            RunAtLoad = true;
            KeepAlive = true;
            EnvironmentVariables = {
              PATH =
                "/usr/bin/:/sbin:/bin:/usr/local/bin:"
                + lib.makeBinPath (with pkgs; [ cfg.package ] ++ cfg.extraPackages);
            };
          };
        };
        home.packages = [ cfg.package ];
        #          auscybernix.keybinds.karabiner-driver-kit.karabinerPackage = pkgs.karabiner-elements;
      })

    ]
  );

}
