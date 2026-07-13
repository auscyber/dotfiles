{
  lib,
  stdenvNoCC,
  unzip,
  jq,
}:

# Zotero identifies a plugin by `manifest.applications.zotero.id` (XPIInstall.sys.mjs)
# and, when scanning <profile>/extensions, derives the id from the *filename*
# (XPIProvider.sys.mjs: id = leafName minus ".xpi"). So the released xpi must be
# installed verbatim under `<id>.xpi` — repacking it (as nixpkgs' fetchFirefoxAddon
# does) rewrites `applications` to the gecko form and destroys the id Zotero reads.
# Signing is irrelevant: Zotero sets xpinstall.signatures.required = false.

{
  pname,
  version,
  src,
  addonId,
  meta ? { },
}:

stdenvNoCC.mkDerivation {
  inherit pname version src;

  dontUnpack = true;
  nativeBuildInputs = [
    unzip
    jq
  ];

  installPhase = ''
    runHook preInstall

    manifestId=$(unzip -p "$src" manifest.json | jq -er '.applications.zotero.id')
    if [ "$manifestId" != ${lib.escapeShellArg addonId} ]; then
      echo "fetchZoteroAddon: manifest declares id '$manifestId', but addonId is '${addonId}'." >&2
      echo "The file must be named <id>.xpi or Zotero will not load it." >&2
      exit 1
    fi

    install -Dm444 "$src" "$out/share/zotero/extensions/$manifestId.xpi"

    runHook postInstall
  '';

  passthru = { inherit addonId; };

  meta = {
    platforms = lib.platforms.all;
    sourceProvenance = [ lib.sourceTypes.binaryBytecode ];
  } // meta;
}
