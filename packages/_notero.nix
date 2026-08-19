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
  # scripts/utils/version.mts reads gen/version.json when it exists and otherwise
  # computes a local version ending in `-<username>.<hostname>` -- inside the
  # sandbox that's the useless `-nixbld.localhost`. Writing the file first pins
  # the version create-xpi bakes into the manifest and the xpi filename.
  versionTag = "auscyber-nix-${builtins.substring 0 7 source.version}";
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
    hash = "sha256-t1bob6sx46uwhCfCO0rhYoCW4YvbEt2JtDwBbB8VfLk=";
  };

  buildPhase = ''
    runHook preBuild

    # Same patch bump upstream's getLocalVersion applies, so the build outranks
    # the released version in Zotero's update check.
    xpiVersion=$(jq -er '
      .version
      | split(".")
      | "\(.[0]).\(.[1]).\((.[2] | split("-") | .[0] | tonumber) + 1)"
    ' package.json)-${versionTag}

    mkdir -p gen
    jq -n --arg version "$xpiVersion" '$version' > gen/version.json
    echo "notero: pinned xpi version to $xpiVersion"

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

    manifestVersion=$(unzip -p "$xpi" manifest.json | jq -er '.version')
    pinnedVersion=$(jq -er . gen/version.json)
    if [ "$manifestVersion" != "$pinnedVersion" ]; then
      echo "notero: manifest declares version '$manifestVersion', expected '$pinnedVersion'" >&2
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
