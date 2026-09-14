{ den, ... }: {
  den.aspects.niri = {
    # Only `gui` declared; the platform is inferred from the including hosts.
    layers = [ "gui" ];

    # Declared on the aspect, not the file: that is what lets the partition
    # generator see which aspect an input belongs to, and therefore which
    # platforms need it.
    inputs.niri = {
      url = "github:sodiboo/niri-flake";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    homeManager = {
      programs.niri = {
        enable = true;
        settings = { };
      };
    };
  };
}
