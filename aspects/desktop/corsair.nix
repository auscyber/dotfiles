{ den, lib, ... }:
{

  den.aspects.corsair.includes = [ den.aspects.rgb ];
  den.aspects.corsair.gui.nixos =
    { pkgs, user, ... }:
    let
      installDir = "/home/${user.name}/.openlinkhub";
    in

    {

      services.udev.packages = with pkgs; [ openlinkhub ];
      environment.systemPackages = with pkgs; [ openlinkhub ];
      systemd.services.openlinkhub = {
        description = "Open source interface for iCUE LINK System Hub, Corsair AIOs and Hubs";
        wants = [ "network-online.target" ];
        wantedBy = [ "multi-user.target" ];
        requires = [ "openlinkhub-install.service" ];
        after = [
          "network-online.target"
          "local-fs.target"
          "openlinkhub-install.service"
        ];
        startLimitIntervalSec = 60;
        startLimitBurst = 5;
        serviceConfig = {
          ExecStart = lib.getExe pkgs.openlinkhub;
          ExecReload = "/run/current-system/sw/bin/kill -s HUP $MAINPID";
          WorkingDirectory = installDir;
          Restart = "always";
          RestartSec = 5;
          User = user.name;
          Group = "openlinkhub";
          PrivateTmp = true;
          ProtectSystem = "strict";
          ProtectHome = "read-only";
          ReadWritePaths = [ installDir ];
          DeviceAllow = [
            "char-usb_device rw"
            "char-input rw"
            "char-hidraw rw"
          ];
        };
      };
    };
  den.aspects.auspc.includes = [ den.aspects.corsair ];
}
