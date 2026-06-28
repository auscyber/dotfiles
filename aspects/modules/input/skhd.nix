{ den, lib, ... }:
{
  den.aspects.skhd = {
    homeManager =
      { config, ... }:
      let
        binds = config.skhd.binds or { };
      in
      {
        options.skhd.binds = lib.mkOption {
          type = lib.types.attrsOf lib.types.str;
          default = { };
          description = "skhd keybind table: key combo -> shell command.";
        };

        config = {
          services.skhd = {
            enable = true;
            config = lib.concatStringsSep "\n" (
              lib.mapAttrsToList (k: v: "${k} : ${v}") binds
            );
          };
          home.file."keybinds.json".text = builtins.toJSON binds;
        };
      };
  };
}
