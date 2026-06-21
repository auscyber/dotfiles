{
  nvfetcher.sources = {
    helium_macos = {
      src.github = "imputnet/helium-macos";
      fetch.url = "https://github.com/imputnet/helium-macos/releases/download/$ver/helium_$ver_arm64-macos.dmg";
    };
    helium_linux = {
      src.github = "imputnet/helium-linux";
      fetch.tarball = "https://github.com/imputnet/helium-linux/releases/download/$ver/helium-$ver-x86_64_linux.tar.xz";
    };
  };

  den.aspects.packages.helium = {

    overlays = { sources, system, ... }: {
      helium =
        self: super:
        let
          helium = {
            x86_64-linux = super.callPackage ./_linux.nix { source = sources.helium_linux; };
            aarch64-darwin = super.callPackage ./_mac.nix { source = sources.helium_macos; };
          };
        in
        {
          helium = helium.${system};
        };
    };

  };

}
