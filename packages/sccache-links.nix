{
  sccache,
  stdenv,
  unwrappedCC,
  lowPrio,
  makeOverridable,
  overrideCC,
  extraConfig,
  makeWrapper,
  lib,
}:
stdenv.mkDerivation (finalAttrs: {
  pname = "sccache-links";
  version = sccache.version;
  passthru = {
    isClang = unwrappedCC.isClang or false;
    isGNU = unwrappedCC.isGNU or false;
    isCcache = true;
  }
  // builtins.intersectAttrs {
    hardeningUnsupportedFlagsByTargetPlatform = null;
    hardeningUnsupportedFlags = null;
  } unwrappedCC;
  lib = lib.getLib unwrappedCC;
  nativeBuildInputs = [ makeWrapper ];
  # Unwrapped clang does not have a targetPrefix because it is multi-target
  # target is decided with argv0.
  buildCommand =
    let
      targetPrefix =
        if unwrappedCC.isClang or false then
          ""
        else
          (lib.optionalString (
            unwrappedCC ? targetConfig && unwrappedCC.targetConfig != null && unwrappedCC.targetConfig != ""
          ) "${unwrappedCC.targetConfig}-");
    in
    ''
      mkdir -p $out/bin

      wrap() {
        local cname="${targetPrefix}$1"
        if [ -x "${unwrappedCC}/bin/$cname" ]; then
          makeWrapper ${sccache}/bin/sccache $out/bin/$cname \
            --run ${lib.escapeShellArg extraConfig} \
            --add-flags ${unwrappedCC}/bin/$cname
        fi
      }

      wrap cc
      wrap c++
      wrap gcc
      wrap g++
      wrap clang
      wrap clang++

      for executable in $(ls ${unwrappedCC}/bin); do
        if [ ! -x "$out/bin/$executable" ]; then
          ln -s ${unwrappedCC}/bin/$executable $out/bin/$executable
        fi
      done
      for file in $(ls ${unwrappedCC} | grep -vw bin); do
        ln -s ${unwrappedCC}/$file $out/$file
      done
      makeWrapper ${sccache}/bin/sccache \
        --run ${lib.escapeShellArg extraConfig}


    '';

  meta = {
  };
})
