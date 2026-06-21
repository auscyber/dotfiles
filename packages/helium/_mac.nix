{
  stdenvNoCC,
  _7zz,
  source,
  makeWrapper,
}:
stdenvNoCC.mkDerivation {
  inherit (source)
    pname
    version
    src
    ;

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
    		      cp -r Helium.app $out/Applications/
    		      runHook postInstall
    		    makeWrapper $out/Applications/Helium.app/Contents/MacOS/helium $out/bin/helium
  '';
}
