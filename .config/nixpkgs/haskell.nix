{pkgs, ...}:
pkgs.haskellPackages.override {
    overrides = self: super: rec {
    xmonad = self.callCabal2nix "xmonad" /home/auscyber/xmonad {};
    xmonad-contrib = self.callCabal2nix "xmonad-contrib" /home/auscyber/xmonad-contrib/. {}; 
    };
  }

