{
  stdenvNoCC,
  _7zz,
  makeWrapper,
  source,
}:
stdenvNoCC.mkDerivation {
  inherit (source) pname version src;

  nativeBuildInputs = [
    _7zz
    makeWrapper
  ];

  sourceRoot = ".";

  dontBuild = true;
  dontFixup = true;

  installPhase = ''
    runHook preInstall
    mkdir -p $out/Applications
    cp -r Cotabby/Cotabby.app $out/Applications/
    makeWrapper $out/Applications/Cotabby.app/Contents/MacOS/Cotabby $out/bin/cotabby
    runHook postInstall
  '';
}
