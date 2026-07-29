# The nvfetcher sources + post-fetch `script` mechanism now lives in the shared
# `ivixlib` repo, delivered as a flake-parts flakeModule plus the denful `ivixlib`
# namespace. dendritic consumes it here for its OWN sources (proton-ge-bin,
# pam_rssh, vscode-kanata, …); ivylix consumes the exact same mechanism — so both
# the dotfiles and ivylix share ivixlib.
#
# This replaces the ~300-line local copy (options.nvfetcher.sources, the `sources`
# module-arg battery, withExtra, and the update-sources/postprocess-sources apps),
# and drops the local `patchedInputs.nvfetcher` — ivixlib builds the forked
# "custom fetcher" nvfetcher itself.
{ inputs, ... }: {
  imports = [
    inputs.ivixlib.flakeModules.default
    (inputs.den.namespace "ivixlib" inputs.ivixlib)
  ];

  perSystem = { config, ... }: {
    update-hooks.postFlake = {
      update-sources = ''
        # ivixlib's update-sources runs nvfetcher, which authenticates GitHub via
        # an nvchecker keyfile (NVCHECKER_KEYS / -k / $HOME/.config/nvchecker.toml),
        # never from the environment. When a token is exported — e.g. CI's
        # GITHUB_TOKEN — synthesise a keyfile and point NVCHECKER_KEYS at it so the
        # run is authenticated; otherwise update-sources uses its own default.
        token="''${GITHUB_TOKEN:-''${GH_TOKEN:-}}"
        if [ -n "$token" ]; then
          NVCHECKER_KEYS="$(mktemp "''${TMPDIR:-/tmp}/nvchecker-keys.XXXXXX")"
          export NVCHECKER_KEYS
          trap 'rm -f "$NVCHECKER_KEYS"' EXIT
          printf '[keys]\ngithub = "%s"\n' "$token" > "$NVCHECKER_KEYS"
        fi
        ${config.apps.update-sources.program}  # call the shared script from ivixlib
      '';
    };
  };
}
