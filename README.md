While we wait for (or contribute towards)
a Nix feature of seamless application of pathces to a flake input,
this project defines a worflow
for application of patches to an imported git branch
and provides commands that support this workflow
[via flake-parts options](https://flake.parts/options/input-branches.html).

Costs:

- 🤹‍♂️ some mental and operational git related overhead

- 🎈 increase in the repository's size due to storage of foreign objects

- 🔖 limits the types a flake can be fetched as to only Git

Benefits:

- 🐬 single-repo setup; more self-contained, less to keep track of

- 💃 no artificial committing/pushing during development

- 🕺 no typing `--override-input` and no accidentally omitting it

- ⚡ provided scripts save time and improve consistency

## The workflow

A Flake input is commonly a branch in a git repository
(even if used as type `github` or similar).
Assuming we lack the freedom to add commits to that upstream branch,
we can clone the upstream repository,
maintain a branch with our commits on top
and rebase it on the upstream branch at some frequency.
That works, but it incurs some costs.
For one, our project spans across one more repository.
But creating a new repository is not necessary.
We can _import the upstream branch_ into our project's repository.
For example, if the upstream branch is Nixpkgs' `nixpkgs-unstable`
then we can import it as `inputs/nixpkgs` in our repository.

> [!NOTE]
> With some repositories one might hit push limits such as
> [GitHub's](https://docs.github.com/en/get-started/using-git/troubleshooting-the-2-gb-push-limit).
> That is the case with a recent Nixpkgs.

Now our project spans across one branch more.

The Nix CLI's `--override-inputs` flag
allows explicit invocations to be attempted
with a temporary substitute for a particular input.
That seems appropriate in many cases.
With this workflow that flag is not necessary.

An input url points at a path inside the repository:

```nix
inputs.nixpkgs.url = "./inputs/nixpkgs";
```

and at that path
is our project repository nested within itself as a Git submodule
in which the `inputs/nixpkgs` branch (or its detached HEAD) is checked out.
`.gitmodules` should have such lines:

```
[submodule "inputs/nixpkgs"]
  path = inputs/nixpkgs
  url = ./.
```

This setup allows us to edit the input
within the local repository clone during development
and execute Nix flake commands without `--override-input`.

> [!IMPORTANT]
> If the superproject state is clean,
> submodules will be fetched instead of used by path.
> Workaround: `$ touch dirt; git add -N dirt`
> This was reported as [issue 13324](https://github.com/NixOS/nix/issues/13324).

When our flake is fetched remotely,
Nix detects the submodule and fetches its HEAD.

> [!NOTE]
> Our flake can now be fetched only using Git (e.g. no `github:` or similar)
> and with submodules enabled, for example:
>
> ```
> $ nix flake show "git+https://example.com/repo?submodules=1&shallow=1"
> ```

> [!TIP]
> Since our project now depends on multiple branches
> make sure that in addition to pushing the regular branches
> input branches are pushed as well.

> [!TIP]
> Be careful that tools such as linters and formatters
> exclude the path `./inputs`.

## NixOS impurity

By default NixOS uses git metadata in some derivations.
That might not be a problem when Nixpkgs is a "normal" input.
But when Nixpkgs is a path type input then the git metadata
is no longer of the Nixpkgs repository
but of the parent repository.
That is unintended behavior
and results in unexpected change to NixOS derivations.

A NixOS module provided by this flake disables these impurities
at the cost of the absence of some version information.
It is available as `#modules.nixos.pure`.
