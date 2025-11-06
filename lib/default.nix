{ inputs, ... }:

{

  flake.lib = {
    system = import ./systems { inherit inputs; };
    file = import ./file.nix {
      inherit inputs;
      self = ../.;
    };
    overlay = import ./overlay.nix { inherit inputs; };
    extra = import ./extra.nix { inherit inputs; };

  };
}
