{
  inputs,
  lib,
  flake-parts-lib,
  config,
  ...
}:
let

  cfg = config.input-branches;
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

in

{
  config.perSystem =
    { pkgs, ... }:
    if (cfg.vcs == "jj") then
      {
        input-branches = {
          commands = lib.pipe cfg.inputs [
            lib.attrValues
            (map (
              {
                name,
                upstream,
                path_,
                shallow,
              }:
              let
                cdToplevel = ''
                  toplevel=$(git rev-parse --show-toplevel)
                  cd "$toplevel"
                '';
                ensure-upstream = ''
                  if ! git remote get-url ${upstream.name} > /dev/null 2>&1; then
                    jj git remote add ${upstream.name} "${upstream.url}"
                    jj git fetch --remote "${upstream.url}"
                    jj bookmark track ${upstream.ref} --remote ${upstream.name}
                  fi
                '';

                get-input-branch = pkgs.writeShellApplication {
                  name = "get-input-branch";
                  runtimeInputs = [ pkgs.git ];
                  text = ''
                    echo "inputs/${name}"
                  '';
                };
              in
              {
                setup = pkgs.writeShellApplication {
                  name = "${cmdPrefix.setup}-${name}";
                  runtimeInputs = [
                    pkgs.git
                    get-input-branch
                  ];
                  text = ''
                    set -o xtrace
                    ${cdToplevel}
                    branch="$(get-input-branch)"
                    ${ensure-upstream}
                    jj bookmark track --remote "${remoteName}" "$branch"
                    jj workspace add -r "$branch" ${path_} --name "${name}"
                  '';
                };
                init =
                  if (inputs.${name} ? rev) then
                    (pkgs.writeShellApplication {
                      name = "${cmdPrefix.init}-${name}";
                      runtimeInputs = [
                        pkgs.git
                        get-input-branch
                      ];
                      text = ''
                        set -o xtrace
                        ${cdToplevel}

                        branch="$(get-input-branch)"

                        ${ensure-upstream}

                        jj git fetch ${upstream.name} "${inputs.${name}.rev}" --remote ${upstream.name}

                        jj workspace add -r ${inputs.${name}.rev} ${path_} --name "${name}"
                        (
                        	cd "${path_}"
                        	jj b c "$branch"
                        	jj bookmark track --remote "${remoteName}" "$branch"

                        )

                      '';
                    })
                  else
                    null;

                rebase = pkgs.writeShellApplication {
                  name = "${cmdPrefix.rebase}-${name}";
                  runtimeInputs = [
                    pkgs.git
                    get-input-branch
                  ];
                  text = ''
                    set -o xtrace
                    ${cdToplevel}
                    branch="$(get-input-branch)"
                    cd "${path_}"
                    ${ensure-upstream}
                    jj git fetch  ${upstream.name} "${upstream.ref}"
                    jj git fetch --remote ${remoteName} "$branch"
                  ''
                  + (
                    if shallow then
                      lib.concatLines [
                        ''
                          git switch --orphan _input-branches-temp
                          git checkout "${upstream.name}/${upstream.ref}" .
                          git add .
                          git commit --message "squashed upstream $(git rev-parse "${upstream.name}/${upstream.ref}")"
                          git switch "$branch"
                        ''
                        # If this is replaced with a naive `git rebase _input-branches-temp`,
                        # tests might still pass, but in actual usage a failure has been observed,
                        # which I have failed to reproduce in tests.
                        ''
                          git rebase --onto=_input-branches-temp "$(git rev-list --max-parents=0 HEAD)" HEAD
                          git branch --force "$branch" HEAD
                          git switch "$branch"
                          git branch --delete --force _input-branches-temp
                        ''
                      ]
                    else
                      ''
                        jj new "$branch" ${upstream.ref}@${upstream.name}
                        jj describe -m "Rebased $branch on ${upstream.name}/${upstream.ref}"
                        jj b move "$branch"
                        jj git export
                      ''
                  );
                };

                push-force = pkgs.writeShellApplication {
                  name = "${cmdPrefix.push-force}-${name}";
                  runtimeInputs = [
                    pkgs.git
                    get-input-branch
                  ];
                  text = ''
                    jj git push --remote ${remoteName} --bookmark "$(get-input-branch)"
                  '';
                };
              }
            ))

            (lib.foldr (cur: acc: {
              init = acc.init ++ (if cur.init != null then [ cur.init ] else [ ]);
              setup = acc.setup ++ [ cur.setup ];
              rebase = acc.rebase ++ [ cur.rebase ];
              push-force = acc.push-force ++ [ cur.push-force ];
            }) (lib.genAttrs cmdClasses (_: [ ])))

            (lib.mapAttrs (
              cmdClass: classCommands:
              classCommands
              ++ lib.optional (classCommands != [ ]) (
                pkgs.writeShellApplication {
                  name = pluralCmd.${cmdClass};
                  text = lib.pipe classCommands [
                    (map lib.getExe)
                    lib.concatLines
                  ];
                }
              )
            ))

            (commands: {
              inherit (commands)
                init
                rebase
                push-force
                setup
                ;
              all = lib.concatLists (
                with commands;
                [
                  init
                  rebase
                  setup
                  push-force
                ]
              );
            })
          ];
        };
      }
    else
      { };

}
