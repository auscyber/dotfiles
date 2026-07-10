{ lib, ... }:
let
  address = [
    "127.0.0.1"
    27631
  ];

  lspmux = {
    listen = address;
    instance_timeout = 300; # after 5 minutes

    # time in seconds how long to wait between the gc task checks for disconnected
    # clients and possibly starts a timeout task. the value must be at least 1.
    gc_interval = 10; # every 10 seconds
    connect = address; # same as `listen`
    log_filters = "info";

    pass_environment = [ "*" ];

  };

in
{

  den.aspects.lspmux = { user, ... }: {
    overlays = {
      lspmux = [
        (
          self: super: {
            wrapLspMux =
              pkg:

              super.writeShellScriptBin "wrap-lspmux" ''
                #!${super.runtimeShell}/bin/sh
                exec ${super.lspmux}/bin/lspmux client --server-path "${lib.getExe pkg} $@"
              '';

          }
        )
      ];
    };

    homeManager = { pkgs, ... }: {
      home.packages = [
        pkgs.lspmux
      ];
    };
    hmLinux = { config, pkgs, ... }: {
      home.file.".config/lspmux/config.toml".source = pkgs.writers.writeTOML "lspmux.toml" lspmux;

      systemd.user.services.lspmux = {
        description = "lspmux service";
        after = [ "network.target" ];
        wantedBy = [ "default.target" ];
        serviceConfig = {
          ExecStart = "${pkgs.lspmux}/bin/lspmux server";
          Restart = "always";
          RestartSec = 5;
          StandardOutput = "append:/tmp/lspmux.log";
          StandardError = "append:/tmp/lspmux.log";
          Environment = lib.concatStringsSep " " (
            lib.mapAttrsToList (name: value: "${name}=${value}") lspmux.pass_environment
          );
        };
      };
    };

    hmDarwin = { config, pkgs, ... }: {

      home.file."Library/Application Support/lspmux/config.toml".source =
        pkgs.writers.writeTOML "lspmux.toml" lspmux;

      launchd.agents.lspmux = {
        enable = true;
        config = {

          ProgramArguments = [
            "${pkgs.lspmux}/bin/lspmux"
            "server"
          ];

          EnvironmentVariables = {
            PATH = lib.concatStringsSep ":" [
              "/usr/bin"
              "/bin"
              "/usr/sbin"
              "/sbin"
            ];
          };

          StandardOutPath = "/tmp/lspmux.log";
          StandardErrorPath = "/tmp/lspmux.log";

          KeepAlive = true;
          RunAtLoad = true;

          LimitLoadToSessionType = [
            "Aqua"
            "Background"
            "LoginWindow"
            "StandardIO"
            "System"
          ];
        };
      };
    };

  };
}
