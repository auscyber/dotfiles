{pkgs, ...}:
pkgs.haskellPackages.override {
    overrides = self: super: rec {
    xmonad = self.callCabal2nix "xmonad" /home/auscyber/xmonad {};
    xmonad-contrib = self.callCabal2nix "xmonad-contrib" /home/auscyber/xmonad-contrib/. {}; 
    agda-stdlib = self.callCabal2nix "agda-stdlib-utils" /home/auscyber/agda-stdlib-1.5/. {};
    };
  }

