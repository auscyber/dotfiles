{
  stdenv,
  fetchurl,
  lib,
  unzip,
}:
stdenv.mkDerivation {
  name = "desktoppr";
  version = "0.5-218";
  pname = "desktoppr";
  src = fetchurl {
    url = "https://github.com/scriptingosx/desktoppr/releases/download/v0.5/desktoppr-0.5-218.zip";
    hash = "sha256-Oa9gAQjOaJHYyT5JBUiFCxL1sQP1dqlFBm+GdmLHNNM=";
  };

  buildInputs = [
    unzip
  ];

  unpackPhase = ''
    runHook preUnpack

    unzip $src

    runHook postUnpack
  '';

  installPhase = ''
    runHook preInstall

    mkdir -p $out/bin
    cp -r desktoppr $out/bin

    runHook postInstall
  '';

  meta = with lib; {
    description = "Set the desktop picture on macOS from the command line";
    homepage = "https://github.com/scriptingosx/desktoppr";
    maintainers = with maintainers; [ auscyber ];
    license = licenses.mit;
    platforms = platforms.darwin;
  };

}
