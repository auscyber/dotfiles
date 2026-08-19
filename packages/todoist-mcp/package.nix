{
  lib,
  buildNpmPackage,
  importNpmLock,
  nodejs_24,
  fetchFromGitHub,
}:
# Same shape as packages/jj-mcp-server: upstream ships package-lock.json, so
# `importNpmLock` vendors from the lockfile's own integrity hashes and there is no
# `npmDepsHash` to re-pin on every version bump.
#
# Packaged rather than run as `npx -y @doist/todoist-mcp` because ZeroClaw spawns
# its stdio servers from a launchd agent, and launchd's PATH does not contain the
# nix profile's `npx`. An absolute store path is the only reliable spelling there.
buildNpmPackage (finalAttrs: {
  pname = "todoist-mcp";
  version = "13.0.0";

  src = fetchFromGitHub {
    owner = "Doist";
    repo = "todoist-mcp";
    rev = "v${finalAttrs.version}";
    hash = "sha256-HScbeTz/++Y72XtphGQav/osa3/A32usajHKJPbD03g=";
  };

  npmDeps = importNpmLock { npmRoot = finalAttrs.src; };
  npmConfigHook = importNpmLock.npmConfigHook;

  # `engines` demands node >= 24 / npm >= 11; the default nodejs in this pin is older.
  nodejs = nodejs_24;

  meta = {
    description = "Model Context Protocol server for Todoist";
    homepage = "https://github.com/Doist/todoist-mcp";
    license = lib.licenses.mit;
    mainProgram = "todoist-mcp";
  };
})
