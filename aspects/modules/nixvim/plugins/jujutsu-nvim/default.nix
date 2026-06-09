{
  den.aspects.nixvim.nvim =
    { lib, ... }:
    lib.nixvim.plugins.mkNeovimPlugin {
      name = "jujutsu";
      package = "jujutsu-nvim";
      moduleName = "jujutsu-nvim";
      maintainers = [ lib.maintainers.auscyber ];
      imports = [ ./_other.nix ];

      settingsOptions = {

        #--- @class JJConfig
        #--- @field diff_preset "difftastic"|"diffview"|"codediff"|"none" Diff viewer preset
        #--- @field help_position "center"|"bottom_right" Help window position
        #--- @field keymap table<string, KeymapBinding> Keymap configuration

        diff_preset = lib.mkOption {
          type = lib.types.enum [
            "difftastic"
            "diffview"
            "codediff"
            "none"
          ];
          default = "difftastic";
        };
        help_position = lib.mkOption {
          type = lib.types.enum [
            "center"
            "bottom_right"
          ];
          default = "center";
        };
        keymap = lib.mkOption {
          type = lib.types.nullOr (lib.nixvim.lua-types.tableOf lib.nixvim.lua-types.anything);
          default = null;
        };
      };
    };
}
