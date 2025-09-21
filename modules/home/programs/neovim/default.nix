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
        source = ../../../../.config/nvim;
        recursive = true;
      };
    };
    programs.neovim = {
      enable = true;
      vimAlias = true;
      #    package = pkgs.neovim-nightly;
      #    extraConfig = ''
      #      let g:sqlite_clib_path = "${pkgs.sqlite.out}/lib/libsqlite3.so"
      #    '';
    };
    home.sessionVariables = {
      EDITOR = "nvim";
      editor = "$EDITOR";
    };
    xdg.configFile."nvim/lua/treesitter_compiler.lua".text = ''
      			return "${pkgs.stdenv.cc}/bin/cc"
            	'';
  };
}
