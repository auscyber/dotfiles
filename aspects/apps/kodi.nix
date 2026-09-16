{ den, ... }:
# Kodi as the machine's whole session: X + lightdm + autologin straight into
# it, which is what an appliance in front of a TV wants.
{
  den.aspects.kodi = {
    nixos =
      {
        host,
        lib,
        pkgs,
        ...
      }:
      {
        services.xserver = {
          enable = true;
          desktopManager.kodi = {
            enable = true;
            package = pkgs.kodi.withPackages (p: [
              p.jellyfin
              p.youtube
            ]);
          };
          displayManager.lightdm.enable = true;
        };

        services.displayManager = {
          defaultSession = "kodi";
          autoLogin = lib.mkIf (host.users or { } != { }) {
            enable = true;
            user = lib.head (lib.attrNames host.users);
          };
        };

        # Kodi's own remote/web interface.
        networking.firewall.allowedTCPPorts = [ 8080 ];
      };
  };
}
