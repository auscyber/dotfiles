{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.auscybernix.sudo.agents;
  primaryUser = config.system.primaryUser;
  kanataCfg = lib.attrByPath [
    "home-manager"
    "users"
    primaryUser
    "auscybernix"
    "keybinds"
    "kanata"
  ] null config;
  mergedCommands =
    cfg.commands
    // {
      yabai = [
        "${pkgs.yabai}/bin/yabai"
        "--load-sa"
      ];
    }
    // lib.optionalAttrs (kanataCfg != null && kanataCfg.enable) {
      kanataTray = kanataCfg.tray.command;
      kanata = kanataCfg.kanataCommand;
    };
  sudoersEntries = lib.mapAttrsToList (
    _agentName: commandList:
    "${primaryUser} ALL=(root) SETENV: NOPASSWD: ${lib.concatStringsSep " " commandList}"

  ) mergedCommands;
in

{

  options.auscybernix.sudo.agents = {
    enable = lib.mkEnableOption "Enable sudoers configuration for agents that require elevated permissions.";
    commands = lib.mkOption {
      type = lib.types.attrsOf (lib.types.listOf lib.types.str);
      default = { };
    };
  };

  config = lib.mkIf cfg.enable (
    lib.mkMerge [
      (

        {

          security.sudo.extraConfig = lib.concatStringsSep "\n" sudoersEntries;

        })
    ]
  );
}
