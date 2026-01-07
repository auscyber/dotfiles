{ config, pkgs, ... }:
let
  tld = "ivymect.in";
  hostname = "meet.${tld}";
in
{
  services.jitsi-meet = {
    enable = true;
    config = {
      enableWelcomePage = false;
      prejoinPageEnabled = true;
      defaultLang = "en";
    };
    interfaceConfig = {
      SHOW_JITSI_WATERMARK = false;
      SHOW_WATERMARK_FOR_GUESTS = false;
    };
    hostName = "${hostname}"; # change this to your domain

    nginx.enable = true;
  };

  services.jitsi-videobridge.openFirewall = true;
  services.nginx.virtualHosts."${hostname}" = {
    useACMEHost = "${hostname}";
    enableACME = false;

  };
  services.nginx.virtualHosts."auth.${hostname}" = {
    useACMEHost = "${tld}";
  };
  services.nginx.virtualHosts."recorder.${hostname}" = {
    useACMEHost = "${tld}";
  };
  services.nginx.virtualHosts."guest.${hostname}" = {
    useACMEHost = "${tld}";
  };

}
