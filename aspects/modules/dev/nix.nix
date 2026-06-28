{ den, ... }:
{
  den.aspects.dev-nix = {
    homeManager =
      { pkgs, ... }:
      {
        home.packages = with pkgs; [
          nil
          nixfmt-rfc-style
          cachix
          devenv
        ];
      };
  };
}
