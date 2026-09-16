{
  den,
  inputs,
  ...
}:
# An input may be declared by exactly ONE aspect -- den errors on a duplicate
# even when the specs are byte-identical -- so impermanence gets an aspect of
# its own that ../hosts/secondpc and ../hosts/birdsarentreal include.
{
  den.aspects.impermanence = {
    layers = [ "nixos" ];

    inputs.impermanence.url = "github:nix-community/impermanence";
    inputs.impermanence.inputs.nixpkgs.follows = "nixpkgs";

    # A module FUNCTION: an attrset here is forced while den collects class
    # content, dragging a nixos-layer input into darwin evaluations.
    nixos = _: { imports = [ inputs.impermanence.nixosModules.impermanence ]; };
  };
}
