{
  inputs,
  lib,
  ...
}:
# CI: build every host in parallel and push the results to the self-hosted
# celler cache (cache.ivymect.in/main).
#
#   * The dynamic GitHub matrix (`flake.ciMatrix`) lives in ./ci-matrix.nix, in the
#     `dev` partition -- it is the only part of CI that needs the
#     `nix-github-actions` input.
#   * The build jobs run `nix build` against THIS flake with
#     `accept-flake-config`, so they pick up substituters + trusted-public-keys
#     straight from the flake's nixConfig -- everything derived from
#     aspects/base/celler-keys.json -- with nothing hardcoded in the workflow.
#   * `apps.sync-ci-secrets` mints + uploads the CELLER_TOKEN that
#     auscyber/celler-action pushes with (a fork of ryanccn/attic-action that
#     uses the celler client instead of attic, since celler's upload protocol
#     requires an X-Celler-Nar-Info header that upstream attic doesn't send).
let
  self = inputs.self;

  # Escape hatch: hosts to leave out of CI entirely. Empty by default -- a host
  # that fails to build just goes red on its own job without affecting the rest
  # (that is the whole point of fail-fast: false).
  excludeHosts = [ ];

  systemOf = cfg: cfg.config.nixpkgs.hostPlatform.system;
  # Standalone home-manager configs don't carry `config.nixpkgs.hostPlatform`
  # (useGlobalPkgs disables the nixpkgs module), and their buildable toplevel is
  # `activationPackage`, not `system.build.toplevel` -- so they key off `pkgs`.
  homeSystemOf = cfg: cfg.pkgs.stdenv.hostPlatform.system;
  keep = configs: removeAttrs configs excludeHosts;
in
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

      onThisSystem = lib.filterAttrs (_: cfg: systemOf cfg == system);
      toplevels =
        class: configs:
        lib.mapAttrs' (name: cfg: lib.nameValuePair "${class}-${name}" cfg.config.system.build.toplevel) (
          onThisSystem (keep configs)
        );

      # Same as `toplevels` but for standalone homeConfigurations: keyed off
      # `pkgs` and exposing `activationPackage` (see homeSystemOf above).
      homeToplevels =
        configs:
        lib.mapAttrs' (name: cfg: lib.nameValuePair "home-${name}" cfg.activationPackage) (
          lib.filterAttrs (_: cfg: homeSystemOf cfg == system) (keep configs)
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
      packages =
        toplevels "nixos" (self.nixosConfigurations or { })
        // toplevels "darwin" (self.darwinConfigurations or { })
        // homeToplevels (self.homeConfigurations or { })
        // lib.optionalAttrs supported { celler = inputs'.celler.packages.celler; };

      apps = lib.optionalAttrs supported {
        sync-ci-secrets = {
          type = "app";
          program = lib.getExe syncCiSecrets;
        };
      };
    };
}
