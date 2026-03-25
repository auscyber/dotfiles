{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.auscybernix.keybinds.kmonad;
  inherit (pkgs.stdenv.hostPlatform) isDarwin isLinux;
in
{
  options.auscybernix.keybinds.kmonad = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Enable kmonad service";
    };
    extraPackages = lib.mkOption {
      type = lib.types.listOf lib.types.package;
      default = [ ];
      description = "Extra packages to install when kmonad is enabled";
    };
    config = lib.mkOption {
      type = lib.types.str;
      default = "${pkgs.kmonad}/share/kmonad/kmonad.kbd";
      description = "kmonad config file content";
    };
  };
  config = lib.mkIf cfg.enable (
    lib.mkMerge [
      #      (lib.mkIf (isLinux && !isDarwin) {
      #        services.kmonad = {
      #          enable = true;
      #          defcfg.enable = true;
      #          keyboard.allow-cmd = true;
      #          config = ''
      #            ${cfg.config}
      #          '';
      #        };
      #      })
      (lib.mkIf isDarwin (
        let
          # create defcfg block
          defcfg = ''
                     (defcfg
                     input  (iokit-name)
                     output (kext)
                     fallthrough true
              allow-cmd true
            )
          '';
          # create kmonad config file
          kmonadConfigFile = pkgs.writeText "kmonad.kbd" (defcfg + "\n" + cfg.config);

        in
        {
          # enable karabiner driver
          environment.systemPackages = with pkgs; [ kmonad ];
          auscybernix.keybinds.karabiner-driver-kit.enable = true;
          #          auscybernix.keybinds.karabiner-driver-kit.karabinerPackage =
          #            pkgs.karabiner-elements.overrideAttrs
          #              (old: {
          #                version = "14.13.0";
          #
          #                src = pkgs.fetchurl {
          #                  inherit (old.src) url;
          #                  hash = "sha256-gmJwoht/Tfm5qMecmq1N6PSAIfWOqsvuHU8VDJY8bLw=";
          #                };
          #
          #                dontFixup = true;
          #              });
          #          # create kmonad service
          security.sudo.extraConfig = builtins.readFile (
            pkgs.runCommand "sudoers-kmonad" { } ''
              KMONAD_BIN="${pkgs.kmonad}/bin/kmonad"
              SHASUM=$(sha256sum "$KMONAD_BIN" | cut -d' ' -f1)
              cat <<EOF >"$out"
              ${config.system.primaryUser} ALL=(root)  SETENV: NOPASSWD: sha256:$SHASUM $KMONAD_BIN ${kmonadConfigFile}
              EOF
            ''
          );

          launchd.user.agents.kmonad = {
            serviceConfig.ProgramArguments = [
              "/usr/bin/sudo"
              "-E"
              "${pkgs.kmonad}/bin/kmonad"
              "${kmonadConfigFile}"
            ];
            environment.PATH =
              "/usr/sbin:/bin:/usr/bin:/sbin:/usr/local/bin:"
              + lib.makeBinPath (cfg.extraPackages ++ [ pkgs.kmonad ]);

            serviceConfig.Label = "org.nixos.kmonad";
            serviceConfig.RunAtLoad = true;
            serviceConfig.KeepAlive = true;
            serviceConfig.StandardErrorPath = "/tmp/kmonad.log";
            serviceConfig.StandardOutPath = "/tmp/kmonad.log";
            #            serviceConfig.UserName = config.system.primaryUser;
          };

        }
      ))

    ]
  );
}
