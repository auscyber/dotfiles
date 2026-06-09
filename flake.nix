{
  description = "A very basic flake";

  inputs = {
    #    self.submodules = true;
    den.url = "github:denful/den";

    nixos-hardware.url = "github:NixOS/nixos-hardware";
    flake-parts.url = "github:hercules-ci/flake-parts";

    nixos-mailserver.url = "gitlab:simple-nixos-mailserver/nixos-mailserver";
    darwin.url = "github:nix-darwin/nix-darwin";
    emacs.url = "github:/nix-community/emacs-overlay";
    flake-utils.url = "github:numtide/flake-utils";
    #nixpkgs
    unstable.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    unstable-small.url = "github:nixos/nixpkgs/nixos-unstable-small";
    stable.url = "github:nixos/nixpkgs/nixos-25.11";

    nixpkgs.follows = "unstable";
    flake-file.url = "github:denful/flake-file";

  };

  outputs =
    inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      # Import all *.nix files in the ./aspects directory
      # Except ones that start with '_'
      imports =
        with inputs.nixpkgs.lib;
        ./aspects
        |> fileset.fileFilter (file: file.hasExt "nix" && !hasPrefix "_" file.name)
        |> fileset.toList;

      _module.args.rootPath = ./.;
    };
}
