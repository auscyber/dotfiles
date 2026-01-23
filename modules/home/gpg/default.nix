{
  config,
  pkgs,
  lib,
  ...
}:
let

  #isDarwin = lib.attrsets.hasAttrByPath [ "environment" "darwinConfig" ] options;
  extraSocketEnabled = config.services.gpg-agent.enableExtraSocket;
in
{
  options.services.gpg-agent.socketAddress = lib.mkOption {
    type = lib.types.str;
  };

  config = lib.mkIf extraSocketEnabled (
    lib.mkMerge [
      {
        services.gpg-agent.socketAddress =
          if pkgs.stdenv.hostPlatform.isDarwin then
            config.launchd.agents.gpg-agent.config.Sockets.extra.SockPathName
          else
            config.systemd.user.gpg-agent.config.sockets.gpg-agent-extra;
        programs.gpg.publicKeys = [
          {
            source = ../../../publickey.asc;
          }
        ];
      }

    ]
  );

}
