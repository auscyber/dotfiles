{
  den.aspects.packages.ghostty = {
    overlays = { sources, system, ... }: {
      ghostty =
        self: super:
        let
          ghostty = {
            #    aarch64-darwin = pkgs.callPackage ../packages/ghostty/default.nix { source = sources.ghostty; }; # inputs.my-nur.packages.aarch64-darwin.ghostty-nightly-bin;
            aarch64-darwin = super.ghostty-bin;
            x86_64-linux = super.ghostty;
            aarch64-linux = super.ghostty;
          };
        in
        {

          ghostty = ghostty."${system}";
        };
    };
  };

}
