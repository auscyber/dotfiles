{
  self,
  inputs,
  lib,
  ...
}:
{
  imports = [
  ];
  flake = {

  };
  perSystem =
    { pkgs, system, ... }:
    {
      packages =
        let
          packages = import ../overlays/literal.nix {
            inherit pkgs system inputs;
            self = pkgs;
          };
          detectFlakeScript = ''
            if [ -n "''${NIXFLAKE:-}" ] && [ -f "''${NIXFLAKE}/flake.nix" ]; then
              printf '%s' "''${NIXFLAKE}"
            elif [ -f "$PWD/flake.nix" ]; then
              printf '%s' "$PWD"
            elif git -C "$PWD" rev-parse --show-toplevel >/dev/null 2>&1; then
              git -C "$PWD" rev-parse --show-toplevel
            elif [ -f "$HOME/dotfiles/flake.nix" ]; then
              printf '%s' "$HOME/dotfiles"
            else
              printf '%s' "$PWD"
            fi
          '';
          helperCommon = ''
            set -euo pipefail
            current_hostname="$(hostname -s)"

            if [ -z "''${current_hostname}" ]; then
              echo "Unable to determine hostname" >&2
              exit 1
            fi
          '';
          systemOptionApp = pkgs.writeShellApplication {
            name = "system-option";
            runtimeInputs = with pkgs; [
              nix
              jq
            ];
            text = ''
              ${helperCommon}
              flake_root="$(${detectFlakeScript})"

              option_path="''${1:-}"
              if [ -z "''${option_path}" ]; then
                echo "Usage: system-option <option.path>"
                exit 1
              fi

              if nix eval --raw --impure --expr 'builtins.currentSystem' | grep -q 'darwin'; then
                target="options-doc-darwin"
              else
                target="options-doc-nixos"
              fi

              out_link="$(mktemp -u)"
              nix build "''${flake_root}#''${target}" --out-link "''${out_link}" >/dev/null
              json_path="''${out_link}/options.json"

              jq -r --arg p "''${option_path}" '
                def fmt:
                  if . == null or . == "" then "n/a" else tostring end;
                def optionType:
                  if (.type | type) == "string" then
                    .type
                  else
                    (.type.description // .type.name // "")
                  end;
                def optionDefault:
                  if (.default | type) == "object" then
                    (.default.text // .default.literalExpression // "")
                  else
                    (.default // "")
                  end;

                .[$p]
                | if . == null then
                    error("option not found: " + $p)
                  else
                    [
                      "Option: " + $p,
                      "Type: " + (optionType | fmt),
                      "Default: " + (optionDefault | fmt),
                      "",
                      "Description:",
                      ((.description // "") | fmt)
                    ] | join("\n")
                  end
              ' "''${json_path}"
            '';
          };
          systemHelpApp = pkgs.writeShellApplication {
            name = "system-help";
            runtimeInputs = with pkgs; [
              nix
              jq
              xdg-utils
              pandoc
            ];
            text = ''
              ${helperCommon}
              flake_root="$(${detectFlakeScript})"

              if nix eval --raw --impure --expr 'builtins.currentSystem' | grep -q 'darwin'; then
                cfg_kind="darwinConfigurations"
                target="options-doc-darwin"
              else
                cfg_kind="nixosConfigurations"
                target="options-doc-nixos"
              fi

              out_link="$(mktemp -u)"
              nix build "''${flake_root}#''${target}" --out-link "''${out_link}" >/dev/null
              md_source="''${out_link}/options.md"

              cache_dir="''${XDG_CACHE_HOME:-$HOME/.cache}/auscybernix-docs"
              mkdir -p "''${cache_dir}"
              html_path="''${cache_dir}/system-options.html"
              pandoc -f commonmark -t html5 -s "''${md_source}" -o "''${html_path}"

              open_cmd=""
              if command -v xdg-open >/dev/null 2>&1; then
                open_cmd="xdg-open"
              elif command -v open >/dev/null 2>&1; then
                open_cmd="open"
              fi

              if [ -n "''${NO_OPEN:-}" ] || [ -z "''${open_cmd}" ]; then
                echo "Detected system config: ''${cfg_kind}.''${current_hostname}"
                echo "Flake root: ''${flake_root}"
                echo "HTML docs: ''${html_path}"
                exit 0
              fi

              if "''${open_cmd}" "''${html_path}" >/dev/null 2>&1; then
                echo "Opened: ''${html_path}"
              else
                echo "Unable to launch browser automatically."
                echo "Open this file manually: ''${html_path}"
              fi
            '';
          };
          homeOptionApp = pkgs.writeShellApplication {
            name = "home-option";
            runtimeInputs = with pkgs; [
              nix
              jq
            ];
            text = ''
              ${helperCommon}
              flake_root="$(${detectFlakeScript})"

              option_path="''${1:-}"
              if [ -z "''${option_path}" ]; then
                echo "Usage: home-option <option.path>"
                exit 1
              fi

              out_link="$(mktemp -u)"
              nix build "''${flake_root}#options-doc-home" --out-link "''${out_link}" >/dev/null
              json_path="''${out_link}/options.json"

              jq -r --arg p "''${option_path}" '
                def fmt:
                  if . == null or . == "" then "n/a" else tostring end;
                def optionType:
                  if (.type | type) == "string" then
                    .type
                  else
                    (.type.description // .type.name // "")
                  end;
                def optionDefault:
                  if (.default | type) == "object" then
                    (.default.text // .default.literalExpression // "")
                  else
                    (.default // "")
                  end;

                .[$p]
                | if . == null then
                    error("option not found: " + $p)
                  else
                    [
                      "Option: " + $p,
                      "Type: " + (optionType | fmt),
                      "Default: " + (optionDefault | fmt),
                      "",
                      "Description:",
                      ((.description // "") | fmt)
                    ] | join("\n")
                  end
              ' "''${json_path}"
            '';
          };
          homeHelpApp = pkgs.writeShellApplication {
            name = "home-help";
            runtimeInputs = with pkgs; [
              nix
              jq
              xdg-utils
              pandoc
            ];
            text = ''
              ${helperCommon}
              flake_root="$(${detectFlakeScript})"

              out_link="$(mktemp -u)"
              nix build "''${flake_root}#options-doc-home" --out-link "''${out_link}" >/dev/null
              md_path="''${out_link}/options.md"

              cache_dir="''${XDG_CACHE_HOME:-$HOME/.cache}/auscybernix-docs"
              mkdir -p "''${cache_dir}"
              html_path="''${cache_dir}/home-options.html"
              pandoc -f commonmark -t html5 -s "''${md_path}" -o "''${html_path}"

              open_cmd=""
              if command -v xdg-open >/dev/null 2>&1; then
                open_cmd="xdg-open"
              elif command -v open >/dev/null 2>&1; then
                open_cmd="open"
              fi

              if [ -n "''${NO_OPEN:-}" ] || [ -z "''${open_cmd}" ]; then
                exit 0
              fi

              if "''${open_cmd}" "''${html_path}" >/dev/null 2>&1; then
                echo "Opened: ''${html_path}"
              else
                echo "Unable to launch browser automatically."
                echo "Open this file manually: ''${html_path}"
              fi
            '';
          };
          homeManagerOptionApp = pkgs.writeShellApplication {
            name = "home-manager-option";
            runtimeInputs = [ homeOptionApp ];
            text = ''
              exec home-option "$@"
            '';
          };
          homeManagerHelpApp = pkgs.writeShellApplication {
            name = "home-manager-help";
            runtimeInputs = [ homeHelpApp ];
            text = ''
              exec home-help "$@"
            '';
          };
          helperPackage = pkgs.symlinkJoin {
            name = "config-helper-commands";
            paths = [
              systemOptionApp
              systemHelpApp
              homeOptionApp
              homeHelpApp
              homeManagerOptionApp
              homeManagerHelpApp
            ];
            meta = {
              description = "Host-aware helper commands for system and home options";
              platforms = pkgs.lib.platforms.all;
            };
          };
          extraPackages = {
            config-helper-commands = helperPackage;
            home-manager-option = homeManagerOptionApp;
            home-manager-help = homeManagerHelpApp;
          };

        in
        (pkgs.lib.filterAttrs (_: p: p ? meta && p.meta ? platforms) packages) // extraPackages;

    };
}
