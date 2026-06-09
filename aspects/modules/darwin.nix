{
  flake-file.inputs.darwin.url = "github:auscyber/nix-darwin?ref=inputs/darwin";

  flake.aspects.darwin.darwin = {

    nix.distributedBuilds = true;

    nix.linux-builder = {
      enable = true;
      ephemeral = true;
      systems = [
        "x86_64-linux"
        "aarch64-linux"
      ];
      config.boot.binfmt.emulatedSystems = [ "x86_64-linux" ];
    };
  };

}
