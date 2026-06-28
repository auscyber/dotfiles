{
  flake-file.inputs.darwin.url = "github:nix-darwin/nix-darwin";

  patchedInputs.darwin = {
    #    patches = [
    ##      ../../../patches/darwin/karabiner.patch
    #      ../../../patches/darwin/specialisation.patch
    #    ];
  };
  flake.aspects.darwin.darwin = {

    nix.distributedBuilds = true;

    nix.linux-builder = {
      enable = true;
      ephemeral = true;
      systems = [
        #        "x86_64-linux"
        "aarch64-linux"
      ];
      #      config.boot.binfmt.emulatedSystems = [ "x86_64-linux" ];
    };
  };

}
