{
  lib,
  stdenvNoCC,
  _7zz,
  sourceRoot,
  makeBinaryWrapper,
  source,
  rsync,
  isNightly ? false,
}:

stdenvNoCC.mkDerivation (finalAttrs: {
  inherit (source) pname version src;
  inherit sourceRoot;

  nativeBuildInputs = [
    _7zz
    makeBinaryWrapper
    rsync
  ];
  unpackPhase = ''
    runHook preUnpack
    7zz -snld x $src
    runHook postUnpack
  '';

  postInstall = ''
    mkdir -p $out/Applications
    mv Cotabby.app $out/Applications/
      	'';

  outputs = [
    "out"
  ];

  # Usually the multiple-outputs hook would take care of this, but
  # our manpages are in the .app bundle

  preferLocalBuild = true;
  meta = {
    description = "Fast, native, feature-rich terminal emulator pushing modern features";
    longDescription = ''
      			Ghostty is a terminal emulator that differentiates itself by being
      			fast, feature-rich, and native. While there are many excellent terminal
      			emulators available, they all force you to choose between speed,
      			features, or native UIs. Ghostty provides all three.
      		'';
    mainProgram = "ghostty";
    sourceProvenance = [ lib.sourceTypes.binaryNativeCode ];
    maintainers = with lib.maintainers; [ auscyber ];
    outputsToInstall = [
      "out"
    ];
    platforms = lib.platforms.darwin;
  };
})
