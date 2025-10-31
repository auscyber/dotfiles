{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.auscybernix.sudo.agents;
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
    let
      sudoersEntries = lib.mapAttrsToList (
        agentName: commandList:
        builtins.readFile (
          pkgs.runCommand "sudoers-${agentName}" { } ''
            COMMAND_BIN="${builtins.head commandList}"
            SHASUM=$(sha256sum "$COMMAND_BIN" | cut -d' ' -f1)
            cat << EOF >"$out"
            ${config.system.primaryUser} ALL=(root) SETENV: NOPASSWD: sha256:$SHASUM ${builtins.concatStringsSep " " commandList}
            EOF
          ''
        )
      ) cfg.commands;

    in
    {

      auscybernix.sudo.agents.commands = {
        yabai = [
          "${pkgs.yabai}/bin/yabai"
          "--load-sa"
        ];
        kanata =
          config.home-manager.users.${config.system.primaryUser}.auscybernix.keybinds.kanata.kanataCommand;
      };

      security.sudo.extraConfig = lib.concatStringsSep "\n" sudoersEntries;
    }
  );
}
