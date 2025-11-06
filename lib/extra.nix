{
  inputs,
}:

let
  lib = inputs.nixpkgs.lib;
in
rec {
  overrideDerivation =
    drv: basePkgs: drv.override (builtins.intersectAttrs drv.override.__functionArgs basePkgs);

}
