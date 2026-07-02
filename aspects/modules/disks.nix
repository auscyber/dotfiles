{
  ff.disko-zfs = {
    url = "github:numtide/disko-zfs";
    inputs.nixpkgs.follows = "nixpkgs";
    inputs.flake-parts.follows = "flake-parts";
    inputs.disko.follows = "disko";
  };
  ff.disko.url = "github:nix-community/disko";
}
