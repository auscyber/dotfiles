{
  description = "my xmonad config";
  inputs = {
    xmonad = {
      url = "github:auscyberman/xmonad";
      flake = false;
    };
    xmonad-contrib = {
      url = "github:auscyberman/xmonad-contrib";
      #      url = "/home/auscyber/xmonad-contrib";
      flake = false;
    };
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "nixpkgs/nixpkgs-unstable";

  };
  outputs = { self, nixpkgs, xmonad, xmonad-contrib, flake-utils }:
    let
      cabal2nix = nixpkgs.haskellPackages.callCabal2nix;
      haskellPackages_overlay = (final: prev: {
        haskellPackages = prev.haskellPackages.override {
          overrides = self: super: rec {
            xmonad = self.callCabal2nix "xmonad" xmonad { };
            xmonad-contrib =
              self.callCabal2nix "xmonad-contrib" xmonad-contrib { };
            my-xmonad = self.callCabal2nix "my-xmonad" ./. { };

          };
        };

      });
      pkgs = import nixpkgs {
        overlays = [ haskellPackages_overlay (final: prev: {
          my-xmonad = final.haskellPackages.my-xmonad;
          xmonad = final.haskellPackages.xmonad;
          xmonad-contrib = final.haskellPackages.xmonad-contrib;
        }) ];
      };
    in

    flake-utils.lib.eachDefaultSystem (system: {

      packages.${system} = {
        xmonad = pkgs.xmonad;
        xmonad-contrib = pkgs.xmonad-contrib;
        my-xmonad = pkgs.my-xmonad;
      };

      defaultPackage.${system} = pkgs.my-xmonad;

    });
}


