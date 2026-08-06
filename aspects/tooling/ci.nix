{
  inputs,
  lib,
  ...
}:
# CI: build every host, one job per system type, and push the results to the
# self-hosted celler cache (cache.ivymect.in/main).
#
#   * The `checks` half of `flake.ciMatrix` (./ci-matrix.nix, in the `dev`
#     partition -- the only part of CI that needs the `nix-github-actions`
#     input) is what systems.yml builds: `.#ciMatrix.checks.<system>."<class>-
#     <name>"` per host. systems.yml runs ONE job per system type (x86_64-
#     linux/aarch64-linux/aarch64-darwin), each pointing `.#nix-fast-build`
#     (below) at `.#ciMatrix.checks.<its-one-system>` -- not one job per host
#     -- so every host of a given arch shares a single checkout/install/
#     cache-restore instead of repeating it per host. `ciMatrix.matrix` (a
#     GitHub Actions matrix of runner labels) is still produced by
#     mkGithubMatrix but no longer consumed anywhere.
#   * The build jobs run `nix-fast-build`/`nix build` against THIS flake with
#     `accept-flake-config`, so they pick up substituters + trusted-public-keys
#     straight from the flake's nixConfig -- everything derived from
#     aspects/base/celler-keys.json -- with nothing hardcoded in the workflow.
#   * There used to be `packages.<class>-<name>` aliases for the same
#     derivations here, but enumerating them means reading
#     `self.{nixos,darwin}Configurations`, which forces every platform partition
#     just to list `packages` -- i.e. a Linux `nix build .#nvim` would fetch the
#     homebrew tarballs again. See aspects/framework/partitions.nix.
#   * Each build job pushes to celler via a real Nix `post-build-hook`
#     (../base/caches.nix has the same mechanism for real hosts) instead of a
#     single bulk push at job end, so a result is pushed the moment it's
#     built -- not only if every host in that job's arch finishes. See
#     .github/actions/celler-build-hook.
#   * `apps.sync-ci-secrets` mints + uploads the CELLER_TOKEN that the
#     build-hook and `warm-patched-inputs`' auscyber/celler-action push with (a
#     fork of ryanccn/attic-action that uses the celler client instead of
#     attic, since celler's upload protocol requires an X-Celler-Nar-Info
#     header that upstream attic doesn't send).
{
  perSystem =
    {
      inputs',
      pkgs,
      system,
      ...
    }:
    let
      # This system has a runner (i.e. is a system we actually build for).
      supported = builtins.elem system [
        "x86_64-linux"
        "aarch64-linux"
        "aarch64-darwin"
      ];

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

          # Unchanged by scoping: this secret sets an explicit `rekeyFile`, so
          # its source path is fixed by that, not derived from the secret name.
          # Only the name `agenix generate` is called with picks up the scope.
          src="aspects/base/github_cache_key.age"
          id="aspects/security/gpg-yubikey.pub"

          GH_TOKEN="$(rage -d -i "$id" "secrets/github_token.age")"
          export GH_TOKEN

          # Mint the token the first time (touch your YubiKey). It is a long-lived
          # scoped celler JWT (sub=github, push=main), regenerated only if absent.
          if [ ! -e "$src" ]; then
          	echo "sync-ci-secrets: $src missing, minting it (touch your YubiKey) ..." >&2
          	agenix generate celler/github_cache_key
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
      packages = lib.optionalAttrs supported {
        celler = inputs'.celler.packages.celler;
        # Exposed so CI can run `.#nix-fast-build` directly against this
        # flake's own pinned nixpkgs (matching `.#celler` above) instead of
        # resolving an unpinned registry `nixpkgs#nix-fast-build` -- keeps CI
        # off the flake registry entirely, same as `celler` above.
        nix-fast-build = pkgs.nix-fast-build;
      };

      apps = lib.optionalAttrs supported {
        sync-ci-secrets = {
          type = "app";
          program = lib.getExe syncCiSecrets;
        };
      };
    };
}
