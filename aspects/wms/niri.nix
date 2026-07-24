{ den, ... }: {
  ff.niri = {
    url = "github:sodiboo/niri-flake";
    inputs.nixpkgs.follows = "nixpkgs";
  };

  den.aspects.niri = {
    homeManager = {
      programs.niri = {
        enable = true;
        settings = { };
      };
    };
  };
}
