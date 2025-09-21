{
  config,
  pkgs,
  lib,
  ...
}:

let
  cfg = config.auscybernix.keybinds.karabiner-driver-kit;
  parentAppDir = "/Applications/.karabiner";

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

    system.activationScripts.preActivation.text = ''
            mkdir -p ${parentAppDir}
            # Kernel extensions must reside inside of /Applications, they cannot be symlinks
            cp -R ${cfg.karabinerDriverPackage}/Applications/.Karabiner-VirtualHIDDevice-Manager.app ${parentAppDir}
      	  cp -R "${cfg.karabinerDriverPackage}/Library/Application Support/org.pqrs" "/Library/Application Support/"
    '';
    launchd.daemons.Karabiner-DriverKit-VirtualHIDDeviceClient = {
      serviceConfig.ProgramArguments = [
        "/Library/Application Support/org.pqrs/Karabiner-DriverKit-VirtualHIDDevice/Applications/Karabiner-VirtualHIDDevice-Daemon.app/Contents/MacOS/Karabiner-VirtualHIDDevice-Daemon"
      ];
      serviceConfig.ProcessType = "Interactive";
      serviceConfig.Label = "org.pqrs.Karabiner-DriverKit-VirtualHIDDevice-Daemon";
      serviceConfig.RunAtLoad = true;
      serviceConfig.KeepAlive = true;
    };
    launchd.daemons.start-karabiner-dk = {
      script = ''
        ${parentAppDir}/.Karabiner-VirtualHIDDevice-Manager.app/Contents/MacOS/Karabiner-VirtualHIDDevice-Manager activate
      '';
      serviceConfig.Label = "org.nixos.start-karabiner-dk";
      serviceConfig.RunAtLoad = true;
    };
    launchd.user.agents.activate_karabiner_system_ext = {
      serviceConfig.ProgramArguments = [
        "${parentAppDir}/.Karabiner-VirtualHIDDevice-Manager.app/Contents/MacOS/Karabiner-VirtualHIDDevice-Manager"
        "activate"
      ];
      serviceConfig.RunAtLoad = true;
      managedBy = "auscybernix.keybinds.karabiner-driver-kit.enable";
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
