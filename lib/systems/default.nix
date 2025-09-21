{ inputs }:
{
  mkDarwin = import ./mk-darwin.nix { inherit inputs; };
  mkNixos = import ./mk-nixos.nix { inherit inputs; };
  common = import ./common.nix { inherit inputs; };
  mkHome = import ./mk-home.nix { inherit inputs; };
}
