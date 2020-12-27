with (import <nixpkgs> {});
derivation {
    name = "xmonadctl";
    builder = "${ghc}/bin/ghc";
    args = [ "--make" ./lib/xmonadctl.hs ];
    baseInputs = [haskellPackages.X11 haskellPackages.xmonad haskellPackages.xmonad-contrib];
    system = builtins.currentSystem;

}
