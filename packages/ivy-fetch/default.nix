{

  den.aspects.packages.ivy-fetch = {
    overlays = { sources, ... }: {
      ivy-fetch = self: super: {
        ivy-fetch = super.callPackage ./package.nix { };
      };
    };

  };

}
