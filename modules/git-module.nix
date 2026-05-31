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
  ];
  cmdPrefix = lib.genAttrs cmdClasses (n: "${cmdBase}-${n}");
  pluralCmd = lib.genAttrs cmdClasses (n: "${pluralCmdBase}-${n}");

in

{
  config.perSystem =
    { pkgs, ... }:
    if (cfg.vcs == "git") then
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
                    git remote add ${upstream.name} "${upstream.url}"
                  fi
                '';

                get-input-branch = pkgs.writeShellApplication {
                  name = "get-input-branch";
                  runtimeInputs = [ pkgs.git ];
                  text = ''
                    echo "inputs/$(git branch --show-current)/${name}"
                  '';
                };
              in
              {
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

                        git submodule add "${cfg.repoPath}" "${path_}"
                        branch="$(get-input-branch)"
                        (
                          cd "${path_}"
                          ${ensure-upstream}
                          git fetch ${lib.optionalString shallow "--depth 1"} ${upstream.name} "${inputs.${name}.rev}"
                      ''
                      + (
                        if shallow then
                          ''
                            git switch --orphan "$branch"
                            git checkout "${inputs.${name}.rev}" .
                            git add .
                            git commit --message "squashed upstream ${inputs.${name}.rev}"
                          ''
                        else
                          ''
                            git switch --create "$branch" "${inputs.${name}.rev}"
                          ''
                      )
                      + ''
                        )
                        git config --file .gitmodules submodule.${path_}.url "${cfg.repoPath}"
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
                    if [ -n "$(git status --porcelain)" ]; then
                      exit 70
                    fi
                    ${ensure-upstream}
                    git fetch ${lib.optionalString shallow "--depth 1"} ${upstream.name} "${upstream.ref}"
                    git fetch ${remoteName} "$branch"
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
                        git switch "$branch"
                        git rebase "${upstream.name}/${upstream.ref}"
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
                    set -o xtrace
                    ${cdToplevel}
                    rev=$(git rev-parse HEAD:${cfg.baseDir}/${name})
                    branch="$(get-input-branch)"
                    tracked_rev=$(git rev-parse :"${path_}")
                    cd "${path_}"
                    current_head=$(git rev-parse HEAD)
                    if [ "$current_head" != "$tracked_rev" ]; then
                    	echo "${path_} is not at tracked commit (HEAD: $current_head, Tracked: $tracked_rev)" >&2
                    	exit 70
                    fi

                    if [ -n "$(git status --porcelain)" ]; then
                    	echo "${path_} is dirty" >&2
                    	exit 70
                    fi
                    git update-ref "refs/heads/$branch" "$rev"
                    git push -f ${remoteName} "$branch:$branch"
                  '';
                };
              }
            ))

            (lib.foldr (cur: acc: {
              init = acc.init ++ (if cur.init != null then [ cur.init ] else [ ]);
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
      }
    else
      { };

}
