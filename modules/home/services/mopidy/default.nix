{
  config,
  pkgs,
  lib,
  ...
}:

let
  cfg = config.auscybernix.services.mopidy;
in

{

  options.auscybernix.services.mopidy = {
    enable = lib.mkEnableOption "Mopidy music server";
  };

  config = lib.mkIf cfg.enable {
    #    sops.secrets."mopidy/listenbrainz_token" = { };
    #    sops.templates."listenbrainz.conf".content = ''
    #      [listenbrainz]
    #      token = ${config.sops.placeholder."mopidy/listenbrainz_token"}
    #      		  '';
    #    services.mopidy = {
    #      enable = true;
    #      extraConfigFiles = [ config.sops.templates."listenbrainz.conf".path ];
    #      settings = {
    #        # General settings
    #
    #        # Audio settings
    #        audio = {
    #          output = "autoaudiosink";
    #        };
    #
    #        # Logging settings
    #        tidal = {
    #          enabled = true;
    #          quality = "LOSSLESS";
    #
    #        };
    #      };
    #      extensionPackages = with pkgs; [
    #        mopidy-iris
    #        mopidy-tidal
    #        mopidy-listenbrainz
    #      ];
    #    };
  };

}
