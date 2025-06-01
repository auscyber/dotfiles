{
  nixpkgs,
  config,
  overlays,
  inputs,
  home-manager,
  darwin,
  home-manager-modules,
  modules,
  ...
}:
darwin.lib.darwinSystem {
  specialArgs = { inherit inputs; };
  system = "aarch64-darwin";
  modules = modules ++ [
    ./configuration.nix
    home-manager.darwinModules.home-manager
    {
      home-manager.users.ivypierlot = {
        imports = home-manager-modules;
        home.username = "ivypierlot";
        home.sessionVariables = {
          NH_FLAKE = "/Users/ivypierlot/dotfiles/nixos-config";
        };
      };
      nixpkgs = {

        overlays = overlays ++ [
          (prev: this: {

            ghostty = prev.ghostty-mac;
          }

          )
        ];
        inherit config;
      };
      # Optionally, use home-manager.extraSpecialArgs to pass
      # arguments to home.nix
    }
  ];
}
