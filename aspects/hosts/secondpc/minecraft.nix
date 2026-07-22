{ den, ... }: {
  den.aspects.secondpc.nixos = {
    services.minecraft-server = {
      enable = true;
      eula = true;
      openFirewall = true;
      declarative = true;
      jvmOpts = "-Xmx4096M -Xms2048M";
      serverProperties = {
        server-port = 25565;
        max-players = 20;
        motd = "Super minecraft";
      };
    };
  };
}
