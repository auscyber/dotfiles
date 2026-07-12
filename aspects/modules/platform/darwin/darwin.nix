{
  ff.darwin = {
    url = "github:nix-darwin/nix-darwin";
    inputs.nixpkgs.follows = "nixpkgs";
  };

  # Patches are auto-included from ./patches/darwin/*.patch.
  patchedInputs.darwin = { };
  den.aspects.nix.darwin = {

    nix.distributedBuilds = true;

    nix.linux-builder = {
      enable = true;
      ephemeral = true;
      systems = [
        #      "x86_64-linux"
        "aarch64-linux"
      ];
      #    config.boot.binfmt.emulatedSystems = [ "x86_64-linux" ];
    };
  };

}
