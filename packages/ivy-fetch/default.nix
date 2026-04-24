{
  writeShellApplication,
  viu,
  lib,
  nowplaying-cli,
  stdenv,
}:
writeShellApplication {
  name = "fetch";
  checkPhase = "";
  bashOptions = [ ];
  runtimeInputs = [
    viu
  ]
  ++ lib.optionals stdenv.isDarwin [ nowplaying-cli ];
  text = builtins.readFile ./fetch;
  meta = {
    platforms = lib.platforms.unix;
  };
}
