{
  den.aspects.rgb.gui.nixos = { pkgs, ... }: {
    services.hardware.openrgb = {
      enable = true;
      package = pkgs.openrgb-with-all-plugins;
      motherboard = "amd";
      server.port = 6742;
    };

  };

  den.aspects.rgb.gui.provides.to-users.homeManager = { pkgs, ... }: {
    home.packages = with pkgs; [ pkgs.openrgb-with-all-plugins ];
  };
}
