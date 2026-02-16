{
  lib,
  stdenv,
  fetchFromGitHub,
  cmake,
  SDL2,
  SDL2_mixer,
  SDL2_ttf,
  SDL2_image,
  SDL2_gfx,
}:

stdenv.mkDerivation rec {
  pname = "openclaw";
  version = "unstable-2022-07-13";

  src = fetchFromGitHub {
    owner = "pjasicek";
    repo = "OpenClaw";
    rev = "5ee5740ca98377c76b13b50c84f610b0066a4717";
    hash = "sha256-wWiRz7WZ0W3e4flhG2uy05cjPdECnFf2BvIqXfOlEuU=";
  };

  nativeBuildInputs = [
    cmake
  ];

  buildInputs = [
    SDL2
    SDL2_mixer
    SDL2_ttf
    SDL2_image
    SDL2_gfx
  ];

  cmakeFlags = [
    "-DCMAKE_BUILD_TYPE=Release"
  ];

  installPhase = ''
    runHook preInstall
    mkdir -p $out/bin
    cp openclaw $out/bin/
    runHook postInstall
  '';

  meta = with lib; {
    description = "Reimplementation of Captain Claw (1997) platformer";
    homepage = "https://github.com/pjasicek/OpenClaw";
    license = licenses.gpl3Plus;
    maintainers = with maintainers; [ auscyber ];
    platforms = platforms.linux;
  };
}
