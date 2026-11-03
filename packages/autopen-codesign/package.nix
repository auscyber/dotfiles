{
  lib,
  rustPlatform,
  makeWrapper,
  autopen,
}:
# Signs a Mach-O through an autopen server: the bridge between apple-codesign
# and a remote autopen key, so the signer never holds key material.
rustPlatform.buildRustPackage {
  pname = "autopen-codesign";
  version = "0.1.0";

  src = lib.fileset.toSource {
    root = ./.;
    fileset = lib.fileset.unions [
      ./Cargo.toml
      ./Cargo.lock
      ./src
    ];
  };

  cargoLock.lockFile = ./Cargo.lock;

  nativeBuildInputs = [ makeWrapper ];

  postInstall = ''
    wrapProgram $out/bin/autopen-codesign --set-default AUTOPEN ${lib.getExe autopen}
  '';

  meta = {
    description = "Sign Mach-O binaries with a key held by an autopen server";
    mainProgram = "autopen-codesign";
    platforms = lib.platforms.darwin;
  };
}
