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

  # Lock file generated at update time by the `vscode-kanata` source's `script`
  # (see packages/kanata-ls/default.nix) and baked into
  # _sources/postprocess/vscode-kanata/. Regenerate with `nix run .#postprocess-sources`.
  cargoLock.lockFile = source.output + "/Cargo.lock";
  # copy kanata.src to the build directory so that the build script can find it
  checkFlags = [
    # reason for disabling test
    "--skip=cli_integration::lsp_initialize_via_stdio_works"
    "--skip=lsp_initialize_via_stdio_works"
  ];

  postPatch = ''
    ln -sf ${source.output}/Cargo.lock Cargo.lock
  '';
  meta = with lib; {
    description = "Kanata Language Server";
    license = licenses.mit;
    mainProgram = "kanata-ls";
    maintainers = with maintainers; [ auscyber ];
  };

}
