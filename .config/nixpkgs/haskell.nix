{pkgs, ...}:
pkgs.haskellPackages.override {
    overrides = self: super: rec {
    xmonad = self.callCabal2nix "xmonad" (pkgs.fetchFromGitHub {
      repo = "xmonad";
      owner = "auscyberman";
      rev = "master";
      sha256="sha256-9oEiVz0YQhv/pMfBO3thM9QwNvUN6fWa+HDDzqU3iQI=";
    }) {};
    xmonad-contrib = self.callCabal2nix "xmonad-contrib" (pkgs.fetchFromGitHub {
      repo = "xmonad-contrib";
      owner = "auscyberman";
      rev = "master";
      sha256="sha256-uhtF+Z1dmrwC2nNZRxrOdqQYN8IZ0kxsnZzsD3WHr+E=";
    }){};
    agda-stdlib = self.callCabal2nix "agda-stdlib-utils" /home/auscyber/agda-stdlib-1.5/. {};
    };
  }

