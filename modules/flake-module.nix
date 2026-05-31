{
  flake.flakeModules.default =
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
      pluralCmdBase = "input-branches";
      cmdClasses = [
        "init"
        "rebase"
        "push-force"
        "setup"
      ];
      cmdPrefix = lib.genAttrs cmdClasses (n: "${cmdBase}-${n}");
      pluralCmd = lib.genAttrs cmdClasses (n: "${pluralCmdBase}-${n}");
      upstream-name = name: if cfg.vce == "jj" then "${name}-upstream" else "upstream";
    in
    {
      options = {
        input-branches = {
          vcs = lib.mkOption {
            type = lib.types.enum [
              "git"
              "jj"
            ];
            default = "git";
            description = ''
              Version control system to use for input branches.
              Currently, only Git is supported.
            '';
          };
          repoPath = lib.mkOption {
            type = lib.types.str;
            description = ''
              Path to the Git repository root.
              If not set, it is determined automatically.
            '';
            default = "./.";
          };
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
            defaultText = "{ }";
            description = ''
              Input branch definitions.
              Each attribute name must correspond to an existing flake input.
            '';
            example =
              lib.literalExpression
                # nix
                ''
                  {
                    nixpkgs = {
                      upstream = {
                        url = "https://github.com/NixOS/nixpkgs.git";
                        ref = "nixpkgs-unstable";
                      };
                      shallow = true;
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

                    shallow = lib.mkOption {
                      type = lib.types.bool;
                      default = false;
                      example = true;
                      description = ''
                        Useful for an input with huge history, such as Nixpkgs.
                        Fetching occurs with `--depth 1`.
                        The input branch is initialized with a single, artificial, initial commit.
                        The commit message includes the original upstream rev.
                        Prior to rebasing such a commit is recreated.
                      '';
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
                        default = "${name}-upstream";
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
                  };
                }
              )
            );
          };
        };
        perSystem = flake-parts-lib.mkPerSystemOption {
          options.input-branches = {
            commands = {
              setup = lib.mkOption {
                type = lib.types.listOf lib.types.package;
                readOnly = true;
                description = ''
                  a list of `${cmdPrefix.setup}-<INPUT>` commands
                  that attempt to set up `INPUT` without initializing it.
                  For example, adding the remote and fetching the configured branch.
                  This is useful for CI jobs that want to do some checks before pushing changes to the input branch.
                '';
              };
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
                  It inherited the remote of the remote tracking branch of the current branch.
                  It has an upstream remote according to configuration.
                  The configured branch is checked out
                  and its HEAD set to the rev that the corresponding flake input is locked to.
                  The input url can be set to use it:

                  ```nix
                  {
                    inputs.flake-parts.url = "./${cfg.baseDir}/nixpkgs";
                  }
                  ```

                  An additional command `${pluralCmd.init}` invokes all of these in sequence.
                '';
              };
              rebase = lib.mkOption {
                type = lib.types.listOf lib.types.package;
                readOnly = true;
                description = ''
                  a list of `${cmdPrefix.rebase}-<INPUT>` commands
                  that attempt to rebase `INPUT`

                  An additional command `${pluralCmd.rebase}` invokes all of these in sequence.
                '';
              };
              push-force = lib.mkOption {
                type = lib.types.listOf lib.types.package;
                readOnly = true;
                description = ''
                  a list of `${cmdPrefix.push-force}-<INPUT>` commands
                  that push with `--force` the configured branch of `INPUT`

                  An additional command `${pluralCmd.push-force}` invokes all of these in sequence.
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

      imports = [
        ./git-module.nix
        ./jj-module.nix
      ];
    };
}
