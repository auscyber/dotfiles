{ lib, ... }:
{
  den.default = {
    overlays.ldflags =
      self: super:
      let
        patchLdFlags =
          drv:
          drv.overrideAttrs (oldAttrs: {

            env.NIX_CFLAGS_LINK = "-fuse-ld=lld";
            nativeBuildInputs =
              oldAttrs.nativeBuildInputs or [ ]
              ++ lib.optional super.stdenv.hostPlatform.isDarwin super.llvmPackages.lld;

          });
        drvs = [
          "sketchybar"
          "input-leap"
          "starship"
        ];
      in
      lib.genAttrs drvs (drv: patchLdFlags (super.${drv}));
  };
}
