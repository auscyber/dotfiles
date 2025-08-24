{
  nixpkgs,
  system,
  builder,
  inputs,
  home-manager,
  overlays,
  config,
  modules,
  users,
  ...
}:
builder {
  specialArgs = { inherit inputs; };
  inherit system;

  modules = modules ++ [
    ../modules/common.nix
    {
      home-manager.users = builtins.mapAttrs (
        name: value:
        {
          home.username = name;
        }
        // value
      ) users;
      nixpkgs = { inherit config overlays; };
    }
  ];
}
