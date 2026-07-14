{
  source,
  rustPlatform,
  kanata,
  lib,
  runCommand,
}:
let
  srcWithSibling = runCommand "vscode-kanata-with-kanata" { } ''
    cp -a ${source.src} $out
    chmod -R u+w $out
    rm -rf $out/kanata
    ln -s ${kanata.src} $out/kanata
  '';
in
rustPlatform.buildRustPackage rec {
  inherit (source) version;
  src = srcWithSibling;
  pname = "kanata-ls";
  sourceRoot = "${src.name}/kanata-ls";

  cargoLock.lockFile = ./Cargo.lock;
  # copy kanata.src to the build directory so that the build script can find it
  checkFlags = [
    # reason for disabling test
    "--skip=cli_integration::lsp_initialize_via_stdio_works"
    "--skip=lsp_initialize_via_stdio_works"
  ];

  postPatch = ''
    ln -s ${./Cargo.lock} Cargo.lock
  '';
  meta = with lib; {
    description = "Kanata Language Server";
    license = licenses.mit;
    mainProgram = "kanata-ls";
    maintainers = with maintainers; [ auscyber ];
  };

}
