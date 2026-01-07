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

    home.file = {
      ".config/nvim" = {
        source = config.lib.file.linkLocalPath ../../../../.config/nvim;
        #    recursive = true;
      };
    };
    home.packages = with pkgs; [
      (wrapNeovimUnstable neovim {
        wrapRc = false;
        vimAlias = true;
        withNodeJs = true;
      })
      #      (neovim.override {
      #        vimAlias = true;
      #        withNodeJs = true;
      #      })
    ];

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
