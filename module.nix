{
  inputs,
  lib,
  flake-parts-lib,
  config,
  ...
}:
let
  cfg = config.input-branches;
  # This can probably be wrong in some cases
  remoteName = "origin";
  cmdBase = "input-branch";
  cmdPrefix = lib.genAttrs [ "init" "rebase" "push-force" ] (n: "${cmdBase}-${n}");
in
{
  options = {
    input-branches = {
      baseDir = lib.mkOption {
        type = lib.types.str;
        description = ''
          Directory relative to Git top-level for git submodules.
        '';
        readOnly = true;
        default = "inputs";
      };

      inputs = lib.mkOption {
        default = { };
        defaultText = ''{ }'';
        description = ''
          Input branch definitions.
          Each attribute name must correspond to an existing flake input.
        '';
        example =
          lib.literalExpression
            # nix
            ''
              {
                nixpkgs.upstream = {
                  url = "https://github.com/NixOS/nixpkgs.git";
                  ref = "nixpkgs-unstable";
                };
                home-manager.upstream = {
                  url = "https://github.com/nix-community/home-manager.git";
                  ref = "master";
                };
              }
            '';
        type = lib.types.lazyAttrsOf (
          lib.types.submodule (
            { name, ... }:
            {
              options = {
                name = lib.mkOption {
                  type = lib.types.str;
                  readOnly = true;
                  description = ''
                    Name of input.
                    A flake input by this name must exist.
                  '';
                  example = lib.literalExpression ''"flake-parts"'';
                  default = name;
                  defaultText = lib.literalMD "`<name>`";
                };

                upstream = {
                  url = lib.mkOption {
                    type = lib.types.str;
                    example = lib.literalExpression ''"https://github.com/nix-community/stylix.git"'';
                    description = ''
                      remote URL of the upstream Git repo
                    '';
                  };
                  ref = lib.mkOption {
                    type = lib.types.str;
                    description = ''
                      ref of the upstream Git repo
                    '';
                    example = lib.literalExpression ''"master"'';
                  };
                  name = lib.mkOption {
                    readOnly = true;
                    type = lib.types.str;
                    default = "upstream";
                    description = ''
                      remote upstream name
                    '';
                  };
                };
                path_ = lib.mkOption {
                  type = lib.types.str;
                  readOnly = true;
                  description = ''
                    path of submodule relative to Git top-level
                  '';
                  default = "${cfg.baseDir}/${name}";
                  defaultText = lib.literalMD "`${cfg.baseDir}/<name>`";
                };
                branch = lib.mkOption {
                  type = lib.types.str;
                  readOnly = true;
                  description = ''
                    input branch name
                  '';
                  default = "inputs/${name}";
                  defaultText = lib.literalMD "`inputs/<name>`";
                };
              };
            }
          )
        );
      };
    };
    perSystem = flake-parts-lib.mkPerSystemOption {
      options.input-branches = {
        commands = {
          init = lib.mkOption {
            type = lib.types.listOf lib.types.package;
            readOnly = true;
            description = ''
              A list of `${cmdPrefix.init}-<INPUT>` commands
              that attempt to initialize `INPUT`.
              For example:

              ```
              $ ${cmdPrefix.init}-nixpkgs
              ```

              And you end up with a git submodule at the configured path.
              Refer to `git submodule add` documentation
              for what _the default remote_ ends up being;
              it's commonly but not always `origin`.
              It has an upstream remote according to configuration.
              The configured branch is checked out
              and its HEAD set to the rev that the corresponding flake input is locked to.
              The input url can be set to use it:

              ```nix
              {
                inputs.flake-parts.url = "./${cfg.baseDir}/nixpkgs";
              }
              ```

              __With some repositories one might hit push limits such as
              [GitHub's](https://docs.github.com/en/get-started/using-git/troubleshooting-the-2-gb-push-limit)__.
              That is the case with a recent Nixpkgs.
              Pushing is the last action this command takes,
              so if that fails you can
              step into the directory and try pushing in chunks.
            '';
          };
          rebase = lib.mkOption {
            type = lib.types.listOf lib.types.package;
            readOnly = true;
            description = ''
              a list of `${cmdPrefix.rebase}-<INPUT>` commands
              that attempt to rebase `INPUT`
            '';
          };
          push-force = lib.mkOption {
            type = lib.types.listOf lib.types.package;
            readOnly = true;
            description = ''
              a list of `${cmdPrefix.push-force}-<INPUT>` commands
              that push with `--force` the configured branch of `INPUT`
            '';
          };

          all = lib.mkOption {
            type = lib.types.listOf lib.types.package;
            readOnly = true;
            description = ''
              a list of all of the commands, for convenience
            '';
          };
        };
      };
    };
  };

  config.perSystem =
    { pkgs, ... }:
    {
      input-branches = {
        commands = lib.pipe cfg.inputs [
          lib.attrValues
          (map (
            {
              name,
              upstream,
              path_,
              branch,
            }:
            let
              cdToplevel = ''
                toplevel=$(git rev-parse --show-toplevel)
                cd "$toplevel"
              '';
              ensure-upstream = ''
                if ! git remote get-url ${upstream.name} > /dev/null 2>&1; then
                  git remote add ${upstream.name} "${upstream.url}"
                fi
              '';
            in
            {
              init =
                if (inputs.${name} ? rev) then
                  (pkgs.writeShellApplication {
                    name = "${cmdPrefix.init}-${name}";
                    runtimeInputs = [ pkgs.git ];
                    text = ''
                      set -o xtrace
                      ${cdToplevel}
                      git submodule add "./." "${path_}"
                      cd "${path_}"
                      ${ensure-upstream}
                      git fetch ${upstream.name} "${inputs.${name}.rev}"
                      git switch -c "${branch}" "${inputs.${name}.rev}"
                      git push --set-upstream ${remoteName} "${branch}"
                    '';
                  })
                else
                  null;

              rebase = pkgs.writeShellApplication {
                name = "${cmdPrefix.rebase}-${name}";
                runtimeInputs = [ pkgs.git ];
                text = ''
                  set -o xtrace
                  ${cdToplevel}
                  cd "${path_}"
                  ${ensure-upstream}
                  git fetch ${remoteName} "${branch}"
                  git switch "${branch}"
                  git fetch ${upstream.name} "${upstream.ref}"
                  git rebase "${upstream.name}/${upstream.ref}"
                '';
              };

              push-force = pkgs.writeShellApplication {
                name = "${cmdPrefix.push-force}-${name}";
                runtimeInputs = [ pkgs.git ];
                text = ''
                  set -o xtrace
                  ${cdToplevel}
                  cd "${path_}"
                  ${ensure-upstream}
                  git push -f ${remoteName} "${branch}:${branch}"
                '';
              };
            }
          ))

          (lib.fold (cur: acc: {
            init = acc.init ++ (if cur.init != null then [ cur.init ] else [ ]);
            rebase = acc.rebase ++ [ cur.rebase ];
            push-force = acc.push-force ++ [ cur.push-force ];
          }) (lib.genAttrs [ "init" "rebase" "push-force" ] (_: [ ])))

          (commands: {
            inherit (commands) init rebase push-force;
            all = lib.concatLists (
              with commands;
              [
                init
                rebase
                push-force
              ]
            );
          })
        ];
      };
    };
}
