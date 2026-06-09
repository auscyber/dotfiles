{ den, ... }:
{
  den.aspects.kanata = {
    provides.to-hosts =
      {
        user,
        host,
        ...
      }:
      {
        includes = [ den.aspects.sudoagents ];
        darwin =
          { config, lib, ... }:
          {
            sudoagents.kanata = lib.attrByPath [
              "home-manager"
              "users"
              user.name
              "auscybernix"
              "keybinds"
              "kanata"
              "kanataCommand"
            ] null config;
          };
      };
    homeManager = { pkgs, config, ... }: {
      imports = [ ./_kanata.nix ];
      programs.kanata = {
        enable = true;
        appBundleIds = [ "notion.id" ];
        extraPackages = [
          pkgs.jq
          pkgs.yabai
          pkgs.rift
        ];
        extraCommandPiping = builtins.path {
          name = "kanata-config";
          path = ./tray_config.toml;
        };
        tray = {
          config = {
            "$schema" = "https://raw.githubusercontent.com/rszyma/kanata-tray/main/doc/config_schema.json";

            general = {
              allow_concurrent_presets = false; # (default: false)

              # Optional TCP control server to listen for remote commands, such as stopping/starting a preset.
              # Reference: https://github.com/rszyma/kanata-tray/blob/main/doc/control_server.md
              control_server_enable = true; # (default: false)
            };

            defaults = {
              #kanata_executable = '~/bin/kanata' # if empty or omitted, system $PATH will be searched.
              kanata_config = ""; # if empty or not omitted, kanata default config locations will be used.
              tcp_port = 5829; # (default: 5829)
              autorestart_on_crash = true; # (default: false)

              # Hooks allow running custom commands on specific events (e.g. starting preset).
              # Reference: https://github.com/rszyma/kanata-tray/blob/main/doc/hooks.md

              layer_icons = {
                frenchkeys = "french.ico";
                qwerty = "qwerty.ico";
                "*" = "other_layers.ico";
              };

              presets."main cfg" = {
                autorun = true;
                layer_icons = {
                  frenchkeys = "french.ico";
                };
                # kanata_executable = ''
              };

            };
          };
          # layer_icons = {  }
          # tcp_port = 1234
          # extra_args = ['-n', '-c=~/.config/kanata/another.kbd']

        };
        #      appBundleIds = [
        #        "app.zen-browser.zen"
        #        "com.1password.1password"
        #        "com.1password.1password-launcher"
        #      ];
        config = config.lib.file.getLocalPath ./kanata.kbd;
        extraConfigPaths = [
          #        (
          #          let
          #            splitStuff =
          #              splitted:
          #              builtins.filter (line: line != "") (
          #                lib.splitString "\n" (
          #                  builtins.readFile (
          #                    pkgs.runCommand "toJSON"
          #                      {
          #                        buildInputs = [
          #                          pkgs.perlPackages.JSON
          #                          pkgs.perl
          #                        ];
          #                      }
          #                      ''
          #                        echo '${
          #                          lib.replaceString " " "" splitted
          #                        }' | perl -CSD -0777 -ne 'use utf8; @m = /\X/gu; print join("\n",  @m);' | sed '/^[[:space:]]*$/d' > $out
          #                      ''
          #                  )
          #                )
          #              );
          #
          #            qwerty-fr = builtins.fromJSON (
          #              builtins.readFile (
          #                pkgs.runCommand "toJson" { } ''cat ${../../../qwerty-fr.yaml} | ${lib.getExe pkgs.yq} > $out''
          #              )
          #            );
          #            deadkeys = qwerty-fr.deadkeys;
          #
          #            layerMaps = builtins.map (
          #              layer:
          #              let
          #                base = splitStuff layer.base;
          #                alt = splitStuff layer.alt;
          #                zipped = lib.zipListsWith (
          #                  a: b: "((key-history ${a} 1)) ((unicode \"${b}\") (sequence-noerase 1)) break"
          #                ) base alt;
          #
          #              in
          #              {
          #                name = layer.name;
          #                char = builtins.elemAt (splitStuff layer.char) 1;
          #                output = ''
          #                  (switch
          #                  ${lib.concatStringsSep "\n" zipped}
          #                  _ (use-defsrc) break
          #                  )
          #                '';
          #              }
          #            ) deadkeys;
          #
          #          in
          #          "${pkgs.writeText "qwerty-fr.kbd" ''
          #            (defzippy
          #            ${../../../zippy.txt}
          #
          #              output-character--mappings  (
          #              ${lib.concatStringsSep "\n" (
          #                map (layer: ''
          #                  "${layer.char}" ${layer.output}
          #                '') layerMaps
          #              )}
          #            )
          #            )
          #
          #          ''}"
          #        )

        ];
      };
    };
  };

}
