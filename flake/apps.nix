{
  inputs,
  ...
}:

{

  perSystem =
    { system, pkgs, ... }:
    {
      packages.nixvim =
        let
          nixvimLib = inputs.nixvim.lib.${system};
          nixvim' = inputs.nixvim.legacyPackages.${system};
          nixvimModule = {
            inherit pkgs;
            module = import ../nixvim; # import the module directly
            # You can use `extraSpecialArgs` to pass additional arguments to your module files
            extraSpecialArgs = {
              # inherit (inputs) foo;
            };
          };
          nvim = nixvim'.makeNixvimWithModule nixvimModule;
        in
        nvim;

      apps.fetch = {
        type = "app";
        program = pkgs.writeShellApplication {
          name = "fetch";
          runtimeInputs = [
            inputs.nvfetcher.packages."${system}".default
          ];
          text = ''
            	nvfetcher -k "$HOME/.config/nvchecker.toml"
            	'';
        };

      };
    };
}
