{ den, lib, ... }:
{
  den.aspects.gpg = {
    homeManager =
      { config, pkgs, ... }:
      {
        programs.gpg = {
          enable = true;
          publicKeys = lib.optionals (builtins.pathExists ../../publickey.asc) [
            { source = ../../publickey.asc; }
          ];
        };

        services.gpg-agent = {
          enable = true;
          enableExtraSocket = true;
          enableScDaemon = true;
          extraConfig = ''
            allow-loopback-pinentry
            default-cache-ttl 600
            max-cache-ttl 7200
            ttyname $GPG_TTY
            enable-ssh-support
            debug-level 2
          '';
        } // lib.optionalAttrs pkgs.stdenv.hostPlatform.isDarwin {
          pinentry.package = pkgs.pinentry_mac;
          pinentry.program = "pinentry-mac";
        };

        launchd.agents.gpg-agent = lib.mkIf pkgs.stdenv.hostPlatform.isDarwin {
          config = {
            StandardOutPath = "${config.home.homeDirectory}/.local/share/gpg-agent.log";
            StandardErrorPath = "${config.home.homeDirectory}/.local/share/gpg-agent-error.log";
          };
        };
      };
  };
}
