{ den, ... }:
{
  den.aspects.sudoagents = {

    darwin =
      { config, lib, ... }:
      {
        options.sudoagents = lib.mkOption {
          type = lib.types.attrsOf (lib.types.nullOr (lib.types.listOf lib.types.str));
          default = { };
          description = "Sudo agent commands keyed by aspect name. Each entry grants NOPASSWD sudo to the given command (with args).";
        };
        config.security.sudo.extraConfig =
          let
            sudoersEntries = lib.mapAttrsToList (
              _agentName: commandList:
              "${config.system.primaryUser} ALL=(root) SETENV: NOPASSWD: ${lib.concatStringsSep " " commandList}"
            ) (lib.filterAttrs (_: v: v != null) config.sudoagents);
          in
          lib.concatStringsSep "\n" sudoersEntries;
      };

  };
}
