{
  lib,
  buildZoteroAddon,
  zip,
}:
# Built in-tree rather than fetched: the whole plugin is two files, and its only
# job is to hand Zotero a password this repo already knows where to find. Only
# the zip is local; the filename constraint and its check live in
# buildZoteroAddon.
let
  addonId = "webdav-password@ivymect.in";
in
buildZoteroAddon {
  pname = "zotero-webdav-password";
  version = "1.0.0";
  src = ./webdav-password-zotero;
  inherit addonId;

  xpi = lib.escapeShellArg "${addonId}.xpi";

  nativeBuildInputs = [ zip ];

  dontConfigure = true;

  buildPhase = ''
    runHook preBuild

    # zip refuses timestamps before 1980 and store paths carry the epoch, so
    # the mtimes are pinned rather than left to fail -- which also makes the
    # xpi byte-identical between builds.
    find . -exec touch -t 198001010000 {} +
    zip -X -q -9 ${lib.escapeShellArg "${addonId}.xpi"} manifest.json bootstrap.js

    runHook postBuild
  '';

  meta = {
    description = "Set Zotero's WebDAV sync password from a file at startup";
    license = lib.licenses.mit;
  };
}
