{
  lib,
  stdenvNoCC,
  nodejs,
  pnpm_10,
  fetchPnpmDeps,
  pnpmConfigHook,
  unzip,
  jq,
  source,
}:

# Notero is built from the repo rather than installed from a release xpi.
# `pnpm create-xpi` drops a versioned xpi into ./xpi; it gets installed under the
# same layout fetchZoteroAddon uses (share/zotero/extensions/<addon-id>.xpi), so
# the two are interchangeable in a profile's extensions env.

let
  addonId = "notero@vanoni.dev";
in
stdenvNoCC.mkDerivation (finalAttrs: {
  pname = "zotero-notero";
  # Tracks the tip of main, so `source.version` is a bare commit sha.
  version = "0-unstable-${source.date}";
  inherit (source) src;

  nativeBuildInputs = [
    nodejs
    pnpm_10
    pnpmConfigHook
    unzip
    jq
  ];

  pnpmDeps = fetchPnpmDeps {
    inherit (finalAttrs) pname version src;
    fetcherVersion = 3;
    pnpm = pnpm_10;
    hash = "sha256-6wN50vxhDyKh4cqKl2R0BZZe8WYATAGPBvr5r9Ye1UI=";
  };

  buildPhase = ''
    runHook preBuild

    pnpm run build
    pnpm run create-xpi

    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall

    xpi=$(echo xpi/*.xpi)
    if [ ! -f "$xpi" ]; then
      echo "notero: create-xpi produced no xpi in ./xpi" >&2
      exit 1
    fi

    manifestId=$(unzip -p "$xpi" manifest.json | jq -er '.applications.zotero.id')
    if [ "$manifestId" != ${lib.escapeShellArg addonId} ]; then
      echo "notero: manifest declares id '$manifestId', expected '${addonId}'" >&2
      exit 1
    fi

    install -Dm444 "$xpi" "$out/share/zotero/extensions/$manifestId.xpi"

    runHook postInstall
  '';

  passthru = { inherit addonId; };

  meta = {
    description = "Sync Zotero items into a Notion database";
    homepage = "https://github.com/dvanoni/notero";
    license = lib.licenses.mit;
    platforms = lib.platforms.all;
  };
})
