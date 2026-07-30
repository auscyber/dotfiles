{ den, ... }: {
  den.aspects.niri = {
    # Declared on the aspect, not the file: that is what lets the partition
    # generator see which aspect an input belongs to, and therefore which
    # platforms need it.
    flake-file = _: {
      inputs.niri = {
        url = "github:sodiboo/niri-flake";
        inputs.nixpkgs.follows = "nixpkgs";
      };
    };

    homeManager = {
      programs.niri = {
        enable = true;
        settings = { };
      };
    };
  };
}
