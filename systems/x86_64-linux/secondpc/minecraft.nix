{
  config,
  pkgs,
  lib,
  ...
}:
let
  minecraft-server = pkgs.minecraft-server.overrideAttrs (attrs: {
    src = pkgs.fetchurl {
      url = "https://launcher.mojang.com/v1/objects/a16d67e5807f57fc4e550299cf20226194497dc2/server.jar";
      # sha1 because that comes from mojang via api
      sha1 = "a16d67e5807f57fc4e550299cf20226194497dc2";
    };

  });
in
{
  services.minecraft-server = {
    enable = true;
    eula = true;
    package = minecraft-server;
    openFirewall = true;
    serverProperties = {
      server-port = 25565;
      max-players = 20;
      motd = "Super minecraft";
    };
    declarative = true;
    jvmOpts = "-Xmx4096M -Xms2048M";

  };
}
