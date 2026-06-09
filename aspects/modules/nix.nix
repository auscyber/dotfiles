{
  flake-file.inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/unstable";
    # all your other inputs
  };

  flake.modules.darwin.default = {
    nix.settings.experimental-features = [
      "nix-command"
      "flakes"
      "pipe-operators"
    ];
    nix.optimise = {
      automatic = true;
      interval = [
        {
          Hour = 4;
          Minute = 15;
          Weekday = 7;
        }
      ];
    };
  };
  flake-file.nixConfig = {
    experimental-features = [
      "nix-command"
      "flakes"
      "pipe-operators"
    ];
    extra-substituters = [
      "https://cache.ivymect.in/main"
    ];
    extra-trusted-public-keys = [
      "main:4PgSIjmT7n9adSn4hDnnKXoERhCZR1dTlvj74k+6vT0="
    ];
  };

}
