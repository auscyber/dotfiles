{ den, ... }:
{
  # ── Keybinds aspect ───────────────────────────────────────────────────────────
  # Home-manager: Kanata keyboard remapper (Linux + macOS), Skhd (macOS).
  # Darwin system: KMonad (macOS via Karabiner driver), Karabiner-DK service,
  #                sudoers entries for Kanata.
  # Include in a user/host aspect to deploy keyboard remapping tools.
  den.aspects.keybinds = {
    homeManager =
      { config, lib, pkgs, ... }:
      let
        cfgKanata = config.auscybernix.keybinds.kanata;
        cfgSkhd = config.auscybernix.keybinds.skhd;
        inherit (pkgs.stdenv.hostPlatform) isDarwin isLinux;
        outputFile = pkgs.writeText "kanata-config.kbd" ''
          (include ${cfgKanata.config})
          ${lib.concatStringsSep "\n" (map (p: "(include " + p + ")") cfgKanata.extraConfigPaths)}
        '';
        defaultTrayConfig = pkgs.writers.writeTOML "kanata-tray-config"
          (lib.recursiveUpdate cfgKanata.tray.config {
            defaults.kanata_config = "~/.config/kanata/kanata.kbd";
            defaults.autorestart_on_crash = true;
            defaults.autorun = true;
          });
      in
      {
        options.auscybernix.keybinds.kanata = with lib.types; {
          enable = lib.mkOption { type = bool; default = false; description = "Enable kanata."; };
          extraPackages = lib.mkOption { type = listOf package; default = [ ]; };
          kanataPort = lib.mkOption { type = int; default = 5829; };
          appBundleIds = lib.mkOption { type = listOf str; default = [ ]; };
          package = lib.mkOption { type = package; default = pkgs.kanata-with-cmd; };
          config = lib.mkOption {
            type = str;
            default = "${pkgs.kanata}/share/kanata/kanata.kbd";
            description = "Path to the base kanata config file.";
          };
          extraConfigPaths = lib.mkOption { type = listOf path; default = [ ]; };
          kanataCommand = lib.mkOption { type = listOf str; default = [ "" ]; };
          tray = {
            package = lib.mkPackageOption pkgs "kanata-tray" { };
            command = lib.mkOption { type = listOf str; default = [ "" ]; };
            config = lib.mkOption { type = attrs; default = ""; };
            configFile = lib.mkOption { type = path; default = defaultTrayConfig; };
          };
          extraCommandPiping = lib.mkOption { type = nullOr path; default = null; };
        };
        options.auscybernix.keybinds.skhd = {
          enable = lib.mkOption { type = lib.types.bool; default = false; };
          binds = lib.mkOption { type = lib.types.attrsOf lib.types.str; default = { }; };
        };

        config = lib.mkMerge [
          # ── Kanata (shared) ───────────────────────────────────────────────
          (lib.mkIf cfgKanata.enable (lib.mkMerge [
            # Linux
            (lib.mkIf isLinux {
              systemd.user.services.kanata = {
                Unit = {
                  Description = "Kanata keyboard remapper";
                  PartOf = [ config.wayland.systemd.target ];
                  After = [ "network.target" "dbus.service" config.wayland.systemd.target ];
                };
                Install.WantedBy = [ config.wayland.systemd.target ];
                Service.ExecStart =
                  "${cfgKanata.package}/bin/kanata -c '${cfgKanata.config}' -p ${toString cfgKanata.kanataPort}";
                Service.Restart = "no";
              };
              home.packages = [ cfgKanata.package ] ++ cfgKanata.extraPackages;
            })
            # macOS
            (lib.mkIf isDarwin {
              auscybernix.keybinds.kanata.kanataCommand = lib.mkDefault [
                "${cfgKanata.package}/bin/kanata"
                "-p" "${toString cfgKanata.kanataPort}"
                "-c" "${outputFile}"
              ];
              auscybernix.keybinds.kanata.tray.command = lib.mkDefault [
                "${cfgKanata.tray.package}/bin/kanata-tray"
              ];
              home.file.".config/kanata/kanata.kbd".source = outputFile;
              home.file."Library/Application Support/kanata-tray/kanata-tray.toml".source =
                cfgKanata.tray.configFile;
              launchd.agents.kanata-vk-agent = {
                enable = true;
                config = {
                  Label = "org.nixos.kanata-vk-agent";
                  ProgramArguments =
                    [ "${pkgs.kanata-vk-agent}/bin/kanata-vk-agent"
                      "-p" "${toString cfgKanata.kanataPort}"
                      "-b" "${builtins.concatStringsSep "," cfgKanata.appBundleIds}"
                    ]
                    ++ (lib.optionals (cfgKanata.extraCommandPiping != null) [
                      "-e" (builtins.toString cfgKanata.extraCommandPiping)
                    ]);
                  RunAtLoad = true;
                  KeepAlive = { Crashed = true; SuccessfulExit = false; };
                  StandardErrorPath = "/tmp/kanata-vk-agent.err";
                  StandardOutPath = "/tmp/kanata-vk-agent.out";
                };
              };
              launchd.agents.kanata_tray = {
                enable = true;
                config = {
                  ProgramArguments = [ "/usr/bin/sudo" "-E" ] ++ cfgKanata.tray.command;
                  StandardErrorPath = "/tmp/kanata_tray.err";
                  StandardOutPath = "/tmp/kanata_tray.out";
                  RunAtLoad = true;
                  KeepAlive = true;
                  EnvironmentVariables = {
                    KANATA_TRAY_LOG_DIR = "/tmp";
                    PATH = "/usr/bin/:/sbin:/bin:/usr/local/bin:"
                      + lib.makeBinPath ([ cfgKanata.package ] ++ cfgKanata.extraPackages);
                  };
                };
              };
              launchd.agents.kanata = {
                enable = false;
                config = {
                  ProgramArguments = [ "/usr/bin/sudo" "-E" ] ++ cfgKanata.kanataCommand;
                  StandardErrorPath = "/tmp/kanata_tray.err";
                  StandardOutPath = "/tmp/kanata_tray.out";
                  RunAtLoad = true;
                  KeepAlive = true;
                  EnvironmentVariables = {
                    PATH = "/usr/bin/:/sbin:/bin:/usr/local/bin:"
                      + lib.makeBinPath ([ cfgKanata.package ] ++ cfgKanata.extraPackages);
                  };
                };
              };
              home.packages = [ cfgKanata.package ];
            })
          ]))

          # ── Skhd (macOS only) ─────────────────────────────────────────────
          (lib.mkIf cfgSkhd.enable {
            services.skhd = {
              enable = true;
              config =
                let
                  keys = builtins.attrNames cfgSkhd.binds;
                  rows = map (v: "${v} : ${builtins.getAttr v cfgSkhd.binds}") keys;
                in
                builtins.concatStringsSep "\n" rows;
            };
            home.file."keybinds.json".text = builtins.toJSON cfgSkhd.binds;
          })
        ];
      };

    # ── KMonad + Karabiner-DK (macOS system) ─────────────────────────────────
    darwin =
      { config, lib, pkgs, ... }:
      let
        cfgKmonad = config.auscybernix.keybinds.kmonad;
        cfgKdk = config.auscybernix.keybinds.karabiner-driver-kit;
      in
      {
        options.auscybernix.keybinds.kmonad = {
          enable = lib.mkOption { type = lib.types.bool; default = false; description = "Enable KMonad."; };
          extraPackages = lib.mkOption { type = lib.types.listOf lib.types.package; default = [ ]; };
          config = lib.mkOption {
            type = lib.types.str;
            default = "${pkgs.kmonad}/share/kmonad/kmonad.kbd";
          };
        };
        options.auscybernix.keybinds.karabiner-driver-kit = {
          enable = lib.mkOption { type = lib.types.bool; default = false; };
          package = lib.mkOption { type = lib.types.package; default = pkgs.karabiner-dk; };
        };

        config = lib.mkMerge [
          (lib.mkIf cfgKdk.enable {
            services.karabiner-dk = {
              enable = true;
              package = cfgKdk.package;
            };
          })

          (lib.mkIf cfgKmonad.enable (
            let
              defcfg = ''
                (defcfg
                  input  (iokit-name)
                  output (kext)
                  fallthrough true
                  allow-cmd true
                )
              '';
              kmonadConfigFile = pkgs.writeText "kmonad.kbd" (defcfg + "\n" + cfgKmonad.config);
            in
            {
              environment.systemPackages = with pkgs; [ kmonad ];
              auscybernix.keybinds.karabiner-driver-kit.enable = true;
              security.sudo.extraConfig = builtins.readFile (
                pkgs.runCommand "sudoers-kmonad" { } ''
                  KMONAD_BIN="${pkgs.kmonad}/bin/kmonad"
                  SHASUM=$(sha256sum "$KMONAD_BIN" | cut -d' ' -f1)
                  cat <<EOF >"$out"
                  ${config.system.primaryUser} ALL=(root) SETENV: NOPASSWD: sha256:$SHASUM $KMONAD_BIN ${kmonadConfigFile}
                  EOF
                ''
              );
              launchd.user.agents.kmonad = {
                serviceConfig = {
                  ProgramArguments = [
                    "/usr/bin/sudo" "-E"
                    "${pkgs.kmonad}/bin/kmonad"
                    "${kmonadConfigFile}"
                  ];
                  Label = "org.nixos.kmonad";
                  RunAtLoad = true;
                  KeepAlive = true;
                  StandardErrorPath = "/tmp/kmonad.log";
                  StandardOutPath = "/tmp/kmonad.log";
                };
                environment.PATH =
                  "/usr/sbin:/bin:/usr/bin:/sbin:/usr/local/bin:"
                  + lib.makeBinPath (cfgKmonad.extraPackages ++ [ pkgs.kmonad ]);
              };
            }
          ))
        ];
      };
  };
}
