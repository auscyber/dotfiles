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

    home.file = {
      ".config/nvim" = {
        source = config.lib.file.linkLocalPath ../../../../.config/nvim;
        #    recursive = true;
      };
    };
    home.packages = with pkgs; [ neovim ];

    home.sessionVariables = {
      vim = "nvim";
      EDITOR = "nvim";
      editor = "$EDITOR";
    };
    home.file."${config.auscybernix.flakeConfig.flakeFolder}/.config/nvim/lua/treesitter_compiler.lua".text =
      ''
        			return "${pkgs.stdenv.cc}/bin/cc"
              	'';
  };
}
