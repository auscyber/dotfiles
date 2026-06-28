{ den, ... }:
{
  den.aspects.dev-cli = {
    homeManager =
      { pkgs, ... }:
      {
        home.packages = with pkgs; [
          ripgrep
          shellify
          treefmt
        ];
      };
  };
}
