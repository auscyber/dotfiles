{
  inputs = {
    nixpkgs.follows = "unstable";
    devenv.url = "github:cachix/devenv";

  };
  outputs =
    { self, ... }:
    {

    };
}
