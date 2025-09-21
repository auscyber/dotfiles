{
  config,
  pkgs,
  lib,
  ...
}:

let
  cfg = config.auscybernix.keybinds.karabiner-driver-kit;
  parentAppDir = "/Applications/Nix Apps";

in
{

  options.auscybernix.keybinds.karabiner-driver-kit = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
    };
    karabinerDriverPackage = lib.mkOption {
      type = lib.types.package;
      default = pkgs.karabiner-dk;
    };

  };

  config = lib.mkIf cfg.enable {
    services.karabiner-dk = {
      enable = true;
      package = cfg.karabinerDriverPackage;
    };
    security.sudo.extraConfig =
      lib.mkIf config.home-manager.users.${config.system.primaryUser}.auscybernix.keybinds.kanata.enable
        (
          let
            cfg = config.home-manager.users.${config.system.primaryUser}.auscybernix.keybinds.kanata;
            kanataConfigFile = cfg.config;
          in
          builtins.readFile (
            pkgs.runCommand "sudoers-kanata" { } ''
              KANATA_BIN="${cfg.package}/bin/kanata"
              SHASUM=$(sha256sum "$KANATA_BIN" | cut -d' ' -f1)
              cat <<EOF >"$out"
              ${config.system.primaryUser} ALL=(root)  SETENV: NOPASSWD: sha256:$SHASUM $KANATA_BIN -p ${builtins.toString cfg.kanataPort} -c ${kanataConfigFile}
              EOF
            ''
          )
        );
  };
}
