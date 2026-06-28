{ den, inputs, ... }:
{
  flake-file.inputs.nix-openclaw = {
    url = "github:openclaw/nix-openclaw";
    inputs.nixpkgs.follows = "nixpkgs";
    inputs.home-manager.follows = "home-manager";
  };

  den.aspects.openclaw = {
    homeManager = {
      imports = [ inputs.nix-openclaw.homeManagerModules.openclaw ];
      programs.openclaw.enable = true;
    };
  };
}
