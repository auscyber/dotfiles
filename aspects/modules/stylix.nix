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
      stylix = {
        enable = true;
        image = ../../backgrounds/phoebebridgers-2.jpg;
        polarity = "dark";
        #    base16Scheme = "${pkgs.base16-schemes}/share/themes/darcula.yaml";
      };

    };

    nixos = {
      imports = lib.optional (inputs ? stylix) inputs.stylix.nixosModules.default;
      stylix = {
        enable = true;
        image = ../../backgrounds/phoebebridgers-2.jpg;
        polarity = "dark";
        #    base16Scheme = "${pkgs.base16-schemes}/share/themes/darcula.yaml";
      };

    };
    #    homeManager = {
    #      imports =
    #        lib.optional (inputs ? stylix)
    #
    #          inputs.stylix.homeModules.default;
    #      stylix = {
    #        enable = true;
    #        image = ../../backgrounds/phoebebridgers-2.jpg;
    #        polarity = "dark";
    #        #    base16Scheme = "${pkgs.base16-schemes}/share/themes/darcula.yaml";
    #      };
    #    };

  };
}
