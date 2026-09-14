# GENERATED -- regenerate with `nix run .#write-layer-flakes`.
#
# Placement map: which layer each input belongs to. Consumed by the guarded
# routes in aspects/framework/layers.nix, which is what stops a layer-bound
# input ever being added to the root's `inputs` -- it is not collected there in
# the first place, rather than collected and pruned. lib/layer-inputs.nix then
# merges each layer back at evaluation time from its own lock.
{
  darwin = [
    "coolabah"
    "idris2Packages"
    "paneru"
  ];
  homebrew = [
    "homebrew-cask"
    "homebrew-core"
    "homebrew-gcenx"
    "homebrew-speedtest"
    "homebrew-typewhisper"
    "nix-homebrew"
  ];
  dev = [
    "den-diagram"
    "deploy-rs"
    "nix-github-actions"
  ];
  nixos = [
    "arion"
    "disko"
    "disko-zfs"
    "impermanence"
    "lanzaboote"
    "nix-cachyos-kernel"
    "nix-flatpak"
    "nixos-hardware"
    "nixos-images"
    "nixos-mailserver"
    "nixos-raspberrypi"
    "nixos-wsl"
    "nixpkgs-nvmd"
    "nixvirt"
    "plasma-manager"
    "searchix"
  ];
  packages = [
  ];
}
