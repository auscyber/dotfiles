{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.auscybernix.keybinds.kanata;
  inherit (pkgs.stdenv.hostPlatform) isDarwin isLinux;
  outputFile = pkgs.writeText "kanata-config.kbd" ''
    (include ${cfg.config})
    ${lib.concatStringsSep "\n" (map (path: "(include " + path + ")") cfg.extraConfigPaths)}
  '';
in
{
  options.auscybernix.keybinds.kanata = with lib.types; {
    enable = lib.mkOption {
      type = types.bool;
      default = false;
      description = "Enable kanata service";
    };
    extraPackages = lib.mkOption {
      type = listOf package;
      default = [ ];
      description = "Extra packages to install when kanata is enabled";
    };
    kanataPort = lib.mkOption {
      type = int;
      default = 5829;
    };
    appBundleIds = lib.mkOption {
      type = listOf str;
      default = [ ];
    };
    package = lib.mkOption {
      type = package;
      default = pkgs.kanata-with-cmd;
      description = "kanata package to use";
    };
    config = lib.mkOption {
      type = str;
      default = "${pkgs.kanata}/share/kanata/kanata.kbd";
      description = "kanata config file content";
    };
    extraConfigPaths = lib.mkOption {
      type = listOf str;
      default = [ ];
      description = "Extra config file paths to include";
    };

    kanataCommand = lib.mkOption {
      type = listOf str;
      default = "";
      description = "kanata command to run";
    };
    extraCommandPiping = lib.mkOption {
      type = nullOr path;
      default = null;
      description = "Extra virtual command keys";
    };
  };
  config = lib.mkIf cfg.enable (
    lib.mkMerge [
      (lib.mkIf isLinux {
        systemd.user.services.kanata = {
          Unit = {
            Description = "Kanata keyboard remapper";

            PartOf = [ config.wayland.systemd.target ];
            After = [
              "network.target"
              "dbus.service"
              config.wayland.systemd.target
            ];
          };
          Install = {
            WantedBy = [ config.wayland.systemd.target ];
          };

          Service = {
            ExecStart = "${cfg.package}/bin/kanata -c '${cfg.config}' -p ${builtins.toString cfg.kanataPort}";
            Restart = "no";
            #            Environment = "PATH=$PATH:${lib.makeBinPath ([ cfg.package ] ++ cfg.extraPackages)}";
          };
        };
        home.packages = with pkgs; [ cfg.package ] ++ cfg.extraPackages;
      })
      (lib.mkIf isDarwin {
        # enable karabiner driver
        auscybernix.keybinds.kanata.kanataCommand = lib.mkDefault [

          "${cfg.package}/bin/kanata"
          "-p"
          "${builtins.toString cfg.kanataPort}"
          "-c"
          "${outputFile}"
        ];

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
            ]
            ++ (lib.optionals (cfg.extraCommandPiping != null) [
              "-e"
              (builtins.toString cfg.extraCommandPiping)
            ]);
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
            ]
            ++ cfg.kanataCommand;
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
