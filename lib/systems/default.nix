{ inputs }:
{
  mkDarwin = import ./mk-darwin.nix { inherit inputs; };
  mkNixos = import ./mk-nixos.nix { inherit inputs; };
  rpi =
    let
      rpi = import ./mk-rpi.nix { inherit inputs; };
    in
    {
      mkSystem = args: (rpi args).mkSystem;
      mkInstaller = args: (rpi args).mkInstaller;
    };
  common = import ./common.nix { inherit inputs; };
  mkHome = import ./mk-home.nix { inherit inputs; };
}
