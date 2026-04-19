{
  stdenvNoCC,
  _7zz,
  source,
}:
stdenvNoCC.mkDerivation {
  inherit (source)
    pname
    version
    src
    ;

  nativeBuildInputs = [ _7zz ];

  sourceRoot = ".";

  dontBuild = true;
  dontFixup = true;

  installPhase = ''
    runHook preInstall
    mkdir -p $out/Applications
    cp -r Helium.app $out/Applications/
    runHook postInstall
  '';
}
