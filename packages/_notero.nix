{
  lib,
  buildZoteroAddon,
  nodejs,
  pnpm_10,
  fetchPnpmDeps,
  pnpmConfigHook,
  jq,
  source,
}:
# Notero is built from the repo rather than installed from a release xpi.
# `pnpm create-xpi` drops a versioned xpi into ./xpi; buildZoteroAddon takes it
# from there, so this file only has to produce the xpi.
let
  addonId = "notero@vanoni.dev";
  pname = "zotero-notero";
  # Tracks the tip of main, so `source.version` is a bare commit sha.
  version = "0-unstable-${source.date}";
  # scripts/utils/version.mts reads gen/version.json when it exists and otherwise
  # computes a local version ending in `-<username>.<hostname>` -- inside the
  # sandbox that's the useless `-nixbld.localhost`. Writing the file first pins
  # the version create-xpi bakes into the manifest and the xpi filename.
  versionTag = "auscyber-nix-${builtins.substring 0 7 source.version}";
in
buildZoteroAddon {
  inherit pname version addonId;
  inherit (source) src;

  xpi = "$(echo xpi/*.xpi)";

  nativeBuildInputs = [
    nodejs
    pnpm_10
    pnpmConfigHook
    jq
  ];

  pnpmDeps = fetchPnpmDeps {
    inherit pname version;
    inherit (source) src;
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

  # create-xpi reads gen/version.json too, so a disagreement here means the
  # pinning above silently did not take.
  extraChecks = ''
    manifestVersion=$(unzip -p "$xpi" manifest.json | jq -er '.version')
    pinnedVersion=$(jq -er . gen/version.json)
    if [ "$manifestVersion" != "$pinnedVersion" ]; then
      echo "notero: manifest declares version '$manifestVersion', expected '$pinnedVersion'" >&2
      exit 1
    fi
  '';

  meta = {
    description = "Sync Zotero items into a Notion database";
    homepage = "https://github.com/dvanoni/notero";
    license = lib.licenses.mit;
  };
}
