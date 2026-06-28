{
  writeShellApplication,
  lyricsList,
  viu,
  lib,
  nowplaying-cli,
  stdenv,
}:
writeShellApplication {
  name = "fetch";
  checkPhase = "";
  bashOptions = [ ];
  runtimeEnv.LYRICSLIST = lyricsList;
  runtimeInputs = [ viu ] ++ lib.optionals stdenv.hostPlatform.isDarwin [ nowplaying-cli ];
  text = builtins.readFile ./fetch;
  meta.platforms = lib.platforms.unix;
}
