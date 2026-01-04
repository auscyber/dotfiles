{config, pkgs,...}:
let

    inherit (pkgs.stdenv.hostPlatform) isDarwin;
	in
{
options.services.gpg-agent.socketAddress = {
  type = types.str;
  };

  config = lib.mkIf config.services.gpg-agent.enableExtraSocket (lib.mkMerge [
  (lib.mkIf isDarwin
	{
	  services.gpg-agent.socketAddress = config.launchd.agents.gpg-agent.config.Sockets.Extra.SockPathName;
	}
  )
#  lib.mkIf (!isDarwin)
#  	{
#	services.gpg-agent.socketAddress = "${config.systemd.user.}"


  ] );


}
