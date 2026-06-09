{
  withSystem,
  inputs,
  self,
  ...
}:
{
  flake.darwinConfigurations.Ivys-MacBook-Pro = withSystem "aarch64-darwin" (
    { self', inputs', ... }:
    # TODO: use a common lib for all the systems, (class)
    inputs.darwin.lib.darwinSystem {
      specialArgs = {
        inherit self' inputs';
      };
      modules = with self.modules.darwin; [
        default
        # tokyonight-night
        # gruvbox-dark-hard
      ];
    }
  );
}
