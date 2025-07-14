{
  lib,
  noGitMetadataPlaceholder,
  minimalNixVersionDocSnippet,
  nixosModuleAttr,
  ...
}:
let
  projectName = "input-branches";
  baseDir = "inputs";
  branchPrefix = "inputs/";
  path_ = "README.md";
  text =
    # markdown
    ''
      Apply changes to a flake input by importing its branch and using it as a Git submodule 🤯

      ## Background

      Sometimes, a project can be used as a flake input as-is,
      but often, we want to apply changes to it prior to consumption.
      [Nix does not yet provide a feature of seamless application of patches to flake inputs](https://github.com/NixOS/nix/issues/3920).
      This project defines a workflow for applying changes to an input
      by importing its branch into our repository
      and referring to that branch via path that points to a Git submodule.
      Commands that support this workflow are provided via
      [via flake-parts options](https://flake.parts/options/${projectName}).

      ## Benefits

      - 🐬 single-repo setup; more self-contained, less to keep track of

      - 💃 no artificial committing/pushing during development

      - 🕺 no typing `--override-input` and no accidentally omitting it

      - ⚡ provided scripts save time and improve consistency

      ## The setup

      1. ⬇️ _import the upstream branch_ into our project's own repository.
         For example, we can import the upstream `nixpkgs-unstable` branch as `${branchPrefix}nixpkgs` in our repository.

      2. add our repository at that branch as a Git submodule to our repository 💡.
         For example, at the path `${baseDir}/nixpkgs`. Our `.gitmodules` will include:

         ```
         [submodule "${baseDir}/nixpkgs"]
           path = ${baseDir}/nixpkgs
           url = ./.
         ```

      3. point the input url to that path:

         ```nix
         inputs.nixpkgs.url = "./${baseDir}/nixpkgs";
         ```

      4. Declare `inputs.self.submodules = true`.
         Otherwise, referring to the flake via path (e.g. `nix build .`)
         would result in submodules being omitted.
         It seems that Nix does not support `.?submodules=true`.

      ## Constraints 🪢

      ### Minimal Nix version 🔖

      ${lib.trim minimalNixVersionDocSnippet}

      ### Git fetching only 🐢

      Since submodules need to be fetched,
      the project can be fetched only as a Git repository.
      For example it cannot be referred to as `github:mightyiam/infra` (tarball fetching).
      It must referred to as `git+https://github.com/mightyiam/infra` or similar instead.

      ### The input must be `flake = true` ❄️

      It seems that an input that is referred to
      by relative path inside the superproject (e.g. `./${baseDir}/nixpkgs`)
      and is a Git submodule and is `flake = false`
      ends up being equivalent to `self`, which would be incorrect.
      So input-branch inputs must be `flake = true` (which is the default).
      Also, a `flake.nix` must exist.

      ## Costs 💸

      ### Git submodules overhead 🤹‍♂️

      Just the usual overhead associated with Git submodules,
      such as `$ git submodule init` and `$ git submodule update`.

      ### Superproject must be dirty 🧹

      It seems that when the superproject state is clean,
      submodules that are referred to by path will be fetched via Git 🤕.
      Workaround: `$ touch dirt; git add -N dirt`
      This was reported as [issue 13324](https://github.com/NixOS/nix/issues/13324).

      ### Forge push limits 🚫

      With some repositories one might hit forge push limits such as
      [GitHub's](https://docs.github.com/en/get-started/using-git/troubleshooting-the-2-gb-push-limit).
      That is the case with Nixpkgs.
      These can typically be worked around.

      ### Input branches must be pushed 🫸

      Since our project now depends on multiple branches,
      we must ensure that in addition to pushing the usual branch,
      we also push the input branch.

      ### Tools must consider foreign code 🛠️

      Since we now have input code under `./${baseDir}`,
      we must consider this path in the usage of tools such as linters and formatters.

      ### Git metadata in NiXOS 🏷️

      By default NixOS uses git metadata in some derivations.
      This may be useful under normal circumstances,
      but when Nixpkgs is referred to by path,
      then the git metadata is no longer of the Nixpkgs repository, but of the superproject.
      This unintended consequence also results in more frequent/noisy NixOS derivation change.

      A NixOS module provided by this flake configures NixOS to not use Git metadata.
      It removes the `nixos-version` command
      and replaces the Nixpkgs revision that is by default included in boot entry labels
      with `${noGitMetadataPlaceholder}`.
      It is available as `#modules.nixos.${nixosModuleAttr}`.

      ### Increase in repository size 🦛

      Importing of foreign branches results in storage of foreign objects,
      increasing repository size.
    '';
in
{
  perSystem =
    { pkgs, ... }:
    {
      files.files = [
        {
          inherit path_;
          drv = pkgs.writeText "README.md" text;
        }
      ];
    };
}
