{
  config,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.auscybernix.programs.neovim;

in

{

  options.auscybernix.programs.neovim = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Enable Neovim and configure it with my settings.";
    };
  };
  config = lib.mkIf cfg.enable {
    age.secrets."wakatime_config" = {
      rekeyFile = ../../../../secrets/wakatime_config.age;
      path = "${config.home.homeDirectory}/.wakatime.cfg";
    };

    # Nixvim is already imported via flake importedHomeModules.
    # We keep this module as the toggle point, and source the actual editor config from ./nixvim.
    programs.nixvim = {
      enable = true;
      imports = [ ../../../../nixvim ];
      nixpkgs = {
      inherit pkgs;
      };
          };

    home.sessionVariables = {
      vim = "nvim";
      EDITOR = "nvim";
      editor = "$EDITOR";
    };

    # Old config wrote a treesitter compiler shim for local compilation.
    # nixvim's treesitter build is handled declaratively; keep the file removed.
  };
}
