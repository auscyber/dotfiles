{
  lib,
  rustPlatform,
}:
# Two bins from one crate: `wrapperd` (the boot planter + Mach orchestrator, run
# by the LaunchDaemon) and `wrapperd-wait` (the per-agent shim that gates a
# launchd job on the orchestrator before exec'ing the real signed binary).
# macOS only -- the crate is built on Mach ports and kqueue's EVFILT_MACHPORT and
# does not compile anywhere else.
rustPlatform.buildRustPackage {
  pname = "wrapperd";
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

  meta = {
    description = "Boot-time wrapper planter and Mach-ports launchd ordering orchestrator for darwin";
    mainProgram = "wrapperd";
    license = lib.licenses.mit;
    platforms = lib.platforms.darwin;
  };
}
