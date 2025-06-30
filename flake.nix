{
  outputs = inputs: {
    flakeModules.default = ./module.nix;

    modules.nixos.pure = {
      system = {
        nixos = {
          label = "pure";
          version = "pure";
        };
        tools.nixos-version.enable = false;
      };
    };
  };
}
