{
  den,
  inputs,
  lib,
  ...
}:

{

  flake-file.inputs.stylix.url = "github:nix-community/stylix";

  den.aspects.stylix = {

    darwin = {
      imports = lib.optional (inputs ? stylix) inputs.stylix.darwinModules.default;
    };

    nixos = {
      imports = lib.optional (inputs ? stylix) inputs.stylix.nixosModules.default;
    };
    homeManager = {
      imports =
        lib.optional (inputs ? stylix)

          inputs.stylix.homeManagerModules.default;
    };

  };
}
