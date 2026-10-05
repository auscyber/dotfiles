{
  lib,
  buildZoteroAddon,
}:
# The fetch-by-release wrapper: `src` IS the released xpi, so there is nothing
# to build and the layout, the id assertion and the install all come from
# buildZoteroAddon.
{
  pname,
  version,
  src,
  addonId,
  meta ? { },
}:
buildZoteroAddon {
  inherit
    pname
    version
    src
    addonId
    ;

  dontUnpack = true;

  meta = {
    sourceProvenance = [ lib.sourceTypes.binaryBytecode ];
  }
  // meta;
}
