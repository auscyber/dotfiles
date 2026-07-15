{
  lib,
  rustPlatform,
  source,
}:
rustPlatform.buildRustPackage rec {
  inherit (source) pname src version;
  cargoLock = source.cargoLock."Cargo.lock";
  meta.mainProgram = pname;
}
