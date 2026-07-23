{
  inputs,
  lib,
  ...
}:
# CI: build every host in parallel and push the results to the self-hosted
# celler cache (cache.ivymect.in/main).
#
#   * `flake.ciMatrix` is nix-github-actions' output: `.#ciMatrix.matrix` is a
#     GitHub matrix (one row per host, each system mapped to a runner) that
#     .github/workflows/systems.yml evaluates AT RUN TIME (`nix eval` in a matrix
#     job, `fromJSON` in the build job) -- a proper dynamic matrix, not baked
#     into the YAML. `.#ciMatrix.checks.<system>."<name>"` is the buildable
#     toplevel each row targets.
#   * The build jobs run `nix build` against THIS flake with
#     `accept-flake-config`, so they pick up substituters + trusted-public-keys
#     straight from the flake's nixConfig -- everything derived from
#     aspects/base/celler-keys.json -- with nothing hardcoded in the workflow.
#   * `apps.sync-ci-secrets` mints + uploads the CELLER_TOKEN that
#     ryanccn/attic-action pushes with (celler JWTs are attic-compatible).
let
  self = inputs.self;

  # Escape hatch: hosts to leave out of CI entirely. Empty by default -- a host
  # that fails to build just goes red on its own job without affecting the rest
  # (that is the whole point of fail-fast: false).
  excludeHosts = [ ];

  systemOf = cfg: cfg.config.nixpkgs.hostPlatform.system;
  keep = configs: removeAttrs configs excludeHosts;

  # { <system> = { "<class>-<name>" = <toplevel>; }; } -- the exact shape
  # nix-github-actions.lib.mkGithubMatrix consumes. Its default `platforms` maps
  # each of our systems to a runner (x86_64-linux -> ubuntu-24.04, aarch64-linux
  # -> ubuntu-24.04-arm, aarch64-darwin -> macos-14).
  checksBySystem =
    let
      add =
        class: acc: name: cfg:
        let
          s = systemOf cfg;
        in
        acc
        // {
          ${s} = (acc.${s} or { }) // {
            "${class}-${name}" = cfg.config.system.build.toplevel;
          };
        };
    in
    lib.foldlAttrs (add "darwin") (
      lib.foldlAttrs (add "nixos") { } (keep (self.nixosConfigurations or { }))
    ) (keep (self.darwinConfigurations or { }));
in
{
  ff.nix-github-actions = {
    url = "github:nix-community/nix-github-actions";
    inputs.nixpkgs.follows = "nixpkgs";
  };

  # attrPrefix -> the flake attr each matrix row builds: since mkGithubMatrix
  # returns `{ inherit checks; matrix = {...}; }`, assigning it to flake.ciMatrix
  # makes `.#ciMatrix.checks.<system>."<name>"` a real buildable attribute and
  # `.#ciMatrix.matrix` the runtime-evaluated GitHub matrix.
  flake.ciMatrix = inputs.nix-github-actions.lib.mkGithubMatrix {
    checks = checksBySystem;
    attrPrefix = "ciMatrix.checks";
  };

  perSystem =
    {
      inputs',
      pkgs,
      system,
      ...
    }:
    let
      # This system has a runner (i.e. is a system we actually build for).
      # x86_64-freebsd is in the merged `systems` list but has no hosts and no
      # age-plugin build, so its dev outputs are skipped.
      supported = builtins.elem system [
        "x86_64-linux"
        "aarch64-linux"
        "aarch64-darwin"
      ];

      onThisSystem = lib.filterAttrs (_: cfg: systemOf cfg == system);
      toplevels =
        class: configs:
        lib.mapAttrs' (name: cfg: lib.nameValuePair "${class}-${name}" cfg.config.system.build.toplevel) (
          onThisSystem (keep configs)
        );

      # Same master identity + age plugins the `.#rekey` app uses, so the sync
      # app can decrypt the master-encrypted token source with the YubiKey.
      agenixRekeyPkg = inputs'.agenix-rekey.packages.default;
      agePlugins = [
        (inputs.age-plugin-gpg.packages.${system}.age-plugin-gpg.overrideAttrs (attrs: {
          postInstall = (attrs.postInstall or "") + ''
            ln -s $out/bin/age-plugin-gpg $out/bin/age-plugin-gpg-1
          '';
        }))
        pkgs.rage
      ]
      ++ lib.optionals pkgs.stdenv.isDarwin [ pkgs.age-plugin-se ];

      syncCiSecrets = pkgs.writeShellApplication {
        name = "sync-ci-secrets";
        runtimeInputs = [
          agenixRekeyPkg
          pkgs.gh
          pkgs.coreutils
        ]
        ++ agePlugins;
        text = ''
          set -eu
          if [ ! -e flake.nix ]; then
          	echo "sync-ci-secrets: run from the repo root (flake.nix not found)" >&2
          	exit 1
          fi

          src="aspects/base/github_cache_key.age"
          id="aspects/security/gpg-yubikey.pub"

          GH_TOKEN="$(rage -d -i "$id" "secrets/github_token.age")"
          export GH_TOKEN

          # Mint the token the first time (touch your YubiKey). It is a long-lived
          # scoped celler JWT (sub=github, push=main), regenerated only if absent.
          if [ ! -e "$src" ]; then
          	echo "sync-ci-secrets: $src missing, minting it (touch your YubiKey) ..." >&2
          	agenix generate github_cache_key
          fi

          # Decrypt the master-encrypted source with the same identity `.#rekey`
          # uses. Requires the YubiKey.
          token="$(rage -d -i "$id" "$src")"
          if [ -z "$token" ]; then
          	echo "sync-ci-secrets: decryption produced an empty token" >&2
          	exit 1
          fi

          echo "sync-ci-secrets: uploading CELLER_TOKEN to auscyber/dotfiles ..." >&2
          gh secret set CELLER_TOKEN --repo auscyber/dotfiles --body "$token"
          echo "sync-ci-secrets: done." >&2
        '';
      };
    in
    {
      packages =
        toplevels "nixos" (self.nixosConfigurations or { })
        // toplevels "darwin" (self.darwinConfigurations or { });

      apps = lib.optionalAttrs supported {
        sync-ci-secrets = {
          type = "app";
          program = lib.getExe syncCiSecrets;
        };
      };
    };
}
