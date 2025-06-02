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
  system = "aarch64-darwin";
  modules = modules ++ [
    ./configuration.nix
    home-manager.darwinModules.home-manager
    {
      home-manager.useGlobalPkgs = true;
      home-manager.useUserPackages = true;
      home-manager.users.ivypierlot = {
        imports = home-manager-modules;
        home.username = "ivypierlot";
      };
      nixpkgs = { inherit config overlays; };
      # Optionally, use home-manager.extraSpecialArgs to pass
      # arguments to home.nix
    }
  ];
}
