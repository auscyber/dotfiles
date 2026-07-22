{
  inputs,
  lib,
  ...
}:
# CI: build every host in parallel and push the results to the self-hosted
# celler cache (cache.ivymect.in/main).
#
#   * `flake.ciMatrix` enumerates every host -> { host, class, system, runner,
#     attr } so .github/workflows/systems.yml can fan out one job per machine
#     with `fail-fast: false` -- one machine failing never stops the others.
#   * `packages.<system>.{nixos,darwin}-<host>` exposes each toplevel so a build
#     is `nix build .#nixos-auspc` (and is what the matrix targets).
#   * `apps.sync-ci-secrets` mints the celler CI push token (agenix generator)
#     and uploads it to the repo's GitHub Actions secrets as CELLER_TOKEN, so
#     ryanccn/attic-action in the workflow can push what it builds (celler JWTs
#     are attic-compatible).
let
  self = inputs.self;

  # Escape hatch: hosts to leave out of the CI matrix entirely. Empty by
  # default -- a host that fails to build just goes red on its own job without
  # affecting the rest (that is the whole point of fail-fast: false).
  excludeHosts = [ ];

  # A GitHub-hosted runner that can build each system natively (no emulation).
  runnerFor = {
    "x86_64-linux" = "ubuntu-latest";
    "aarch64-linux" = "ubuntu-24.04-arm";
    "aarch64-darwin" = "macos-latest";
  };

  systemOf = cfg: cfg.config.nixpkgs.hostPlatform.system;
  keep = configs: removeAttrs configs excludeHosts;

  # One matrix row per host. `attr` is the flake attribute the workflow builds;
  # it always exists (den emits nixosConfigurations/darwinConfigurations), so
  # the matrix does not depend on the package aliases below.
  matrixEntries =
    class: configs:
    lib.mapAttrsToList (name: cfg: {
      host = name;
      inherit class;
      system = systemOf cfg;
      runner = runnerFor.${systemOf cfg};
      attr = "${class}Configurations.${name}.config.system.build.toplevel";
    }) (keep configs);
in
{
  ff.github-actions-nix.url = "github:synapdeck/github-actions-nix";
  imports = [
    inputs.github-actions-nix.flakeModule
  ];

  flake.ciMatrix =
    matrixEntries "nixos" (self.nixosConfigurations or { })
    ++ matrixEntries "darwin" (self.darwinConfigurations or { });

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
      # celler/age-plugin build, so its dev outputs are skipped.
      supported = runnerFor ? ${system};

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
