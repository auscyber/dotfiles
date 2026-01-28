{ rustPlatform, lib, source }:
rustPlatform.buildRustPackage rec {
  inherit (source) pname version src;
  cargoDeps = rustPlatform.importCargoLock {
    lockFile = source.cargoLock;
    outputHashes = {
	"continue-0.1.1" = "sha256-9irDEeiPbjIG3e1F/jpWd3fCL1/nei/IYjhMqRp+Q+s=";
	"dispatchr-1.0.0" = "sha256-Df6PdDA5bpmy2P30vGdad+EiHJiANmHrRF2q75Uegik=";
    };
  };

meta = {
platforms = lib.platforms.darwin;
license = lib.licenses.mit;
};
}
