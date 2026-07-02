{
  nvfetcher.sources.alx-wol = {
    src.github = "AndiWeiss/alx-wol";
    fetch.github = "AndiWeiss/alx-wol";
  };

  den.aspects.packages.alx-wol = {
    overlays =
      { sources, ... }:
      {
        alx-wol = self: super: {
          kernelPackagesExtensions = super.kernelPackagesExtensions ++ [

            (self: super: {

              alx-wol = (super.callPackage ./package.nix { }).overrideAttrs ({
                inherit (sources.alx-wol) src version;
              });

            })
          ];

        };
      };
  };
}
