{
  lib,
  stdenvNoCC,
  zip,
  jq,
}:
# Built in-tree rather than fetched: the whole plugin is two files, and its only
# job is to hand Zotero a password this repo already knows where to find.
#
# The xpi MUST be named `<manifest id>.xpi` -- Zotero derives a plugin's id from
# the FILENAME when scanning <profile>/extensions, so a mismatch loads nothing
# and reports no error. Same constraint, and the same build-time check, as
# ./_fetch-zotero-addon.nix.
let
  addonId = "webdav-password@ivymect.in";
in
stdenvNoCC.mkDerivation {
  pname = "zotero-webdav-password";
  version = "1.0.0";
  src = ./webdav-password-zotero;

  nativeBuildInputs = [
    zip
    jq
  ];

  dontConfigure = true;

  buildPhase = ''
    runHook preBuild

    manifestId=$(jq -er '.applications.zotero.id' manifest.json)
    if [ "$manifestId" != ${lib.escapeShellArg addonId} ]; then
      echo "webdav-password: manifest declares id '$manifestId', expected '${addonId}'" >&2
      exit 1
    fi

    # zip refuses timestamps before 1980 and store paths carry the epoch, so
    # the mtimes are pinned rather than left to fail -- which also makes the
    # xpi byte-identical between builds.
    find . -exec touch -t 198001010000 {} +
    zip -X -q -9 "$manifestId.xpi" manifest.json bootstrap.js

    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall
    install -Dm444 ${lib.escapeShellArg "${addonId}.xpi"} \
      "$out/share/zotero/extensions/${addonId}.xpi"
    runHook postInstall
  '';

  passthru = { inherit addonId; };

  meta = {
    description = "Set Zotero's WebDAV sync password from a file at startup";
    license = lib.licenses.mit;
    platforms = lib.platforms.all;
  };
}
