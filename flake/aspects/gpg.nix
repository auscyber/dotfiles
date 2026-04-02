{ den, ... }:
{
  # ── GPG aspect ────────────────────────────────────────────────────────────────
  # Home-manager: GPG extra socket + public key import.
  # Include in a user aspect to configure the GPG agent extra socket and import
  # the user's public key.
  den.aspects.gpg = {
    homeManager =
      { config, lib, pkgs, ... }:
      {
        options.services.gpg-agent.socketAddress = lib.mkOption {
          type = lib.types.str;
          description = "Path to the GPG agent extra socket.";
        };

        config = lib.mkIf config.services.gpg-agent.enableExtraSocket (
          lib.mkMerge [
            {
              services.gpg-agent.socketAddress =
                if pkgs.stdenv.hostPlatform.isDarwin then
                  config.launchd.agents.gpg-agent.config.Sockets.std.SockPathName
                else
                  config.systemd.user.gpg-agent.config.sockets.gpg-agent-extra;
              programs.gpg.publicKeys = [
                { source = ../../publickey.asc; }
              ];
            }
          ]
        );
      };
  };
}
