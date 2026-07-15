{
  lib,
  buildNpmPackage,
  importNpmLock,
  nodejs,
  fetchFromGitHub,
}:
# jj-mcp-server ships its own package-lock.json, so `importNpmLock` vendors the
# dependencies straight from the lockfile's integrity hashes -- the npm analogue of
# rustPlatform's `cargoLock.lockFile`. No `npmDepsHash` to maintain and no fetched
# node deps baked into the tree; nothing has to be *generated* the way kanata-ls's
# missing Cargo.lock does, so this needs neither an nvfetcher post-fetch script nor
# a vendored hash.
buildNpmPackage (finalAttrs: {
  pname = "jj-mcp-server";
  version = "1.0.1";

  src = fetchFromGitHub {
    owner = "keanemind";
    repo = "jj-mcp-server";
    rev = "v${finalAttrs.version}";
    hash = "sha256-GyRitqPvhET+xpMUZteYIuMYoYEssIxelZKz5NJQD+8=";
  };

  npmDeps = importNpmLock { npmRoot = finalAttrs.src; };
  npmConfigHook = importNpmLock.npmConfigHook;

  # Upstream forgets `@types/node` in devDependencies -- its published npm tarball
  # ships the pre-built `build/`, so the broken strict `tsc` build never bites there.
  # Emit without type-checking (TS 5.6+ `--noCheck`) rather than vendoring node types.
  postPatch = ''
    substituteInPlace package.json \
      --replace-fail '"build": "tsc &&' '"build": "tsc --noCheck &&'
  '';

  inherit nodejs;

  meta = {
    description = "Model Context Protocol server for the Jujutsu (jj) version control system";
    homepage = "https://github.com/keanemind/jj-mcp-server";
    license = lib.licenses.mit;
    mainProgram = "jj-mcp-server";
  };
})
