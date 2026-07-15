{ inputs, ... }:

let
  extra = {
    programs.nix-index-database.comma.enable = true;
  };
in
{
  ff = {
    nix-index-database.url = "github:nix-community/nix-index-database";
    nix-index-database.inputs.nixpkgs.follows = "nixpkgs";
  };

  den.aspects.nix-index = {

    nixos = {
      imports = [ inputs.nix-index-database.nixosModules.default ];

    }
    // extra;

    darwin = {
      imports = [ inputs.nix-index-database.darwinModules.nix-index ];
    }
    // extra;
    homeManager = {
      imports = [ inputs.nix-index-database.homeModules.default ];
      programs = {
        inherit (extra.programs) nix-index-database;
        nix-index.enable = true;
      };

    };

  };

}
