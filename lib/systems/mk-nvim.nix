{ inputs, common }:
{
  modules ? [ ],
  name,
}:
let

  flake = inputs.self;
  extendedLib = common.mkExtendedLib flake inputs.nixpkgs;
  finalModules = (extendedLib.importModulesRecursive ../../modules/nixvim) ++ modules;

  nixVimModule = pkgs: {
    inherit pkgs;
    # import the module directly
    extraSpecialArgs = {
      inherit pkgs;
      lib = extendedLib.extend inputs.nixvim.lib.overlay;
    };
  };
  otherModule =
    pkgs:
    nixVimModule pkgs
    // {
      enable = true;
      # add other module options here
      nixpkgs.useGlobalPackages = true;
      imports = finalModules;
    };
in
{
  inherit name;
  derivation =
    { pkgs, system, ... }:
    inputs.nixvim.legacyPackages.${system}.makeNixvimWithModule (
      nixVimModule pkgs
      // {
        module = {
          imports = finalModules;
        };
      }
    );
  homeModule =
    {
      pkgs,
      config,
      lib,
      ...
    }:
    {
      options.auscybernix.nixvim."${name}".enable = lib.mkEnableOption "Enable NixVim";
      config = lib.mkIf config.auscybernix.nixvim."${name}".enable {
        programs.nixvim = otherModule pkgs;
      };
    };
  darwinModule =
    {
      pkgs,
      lib,
      config,
      ...
    }:
    {
      options.auscybernix.nixvim."${name}".enable = lib.mkEnableOption "Enable NixVim";
      config = lib.mkIf config.auscybernix.nixvim."${name}".enable {
        programs.nixvim = (otherModule pkgs);
      };
    };
}
