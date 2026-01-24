{ writeShellApplication, viu,lib }:
writeShellApplication {
  name = "fetch";
  checkPhase = '''';
  bashOptions = [ ];
  runtimeInputs = [ viu ];
  text = builtins.readFile ./fetch;
  meta = {
  platforms = lib.platforms.unix;
  };
}
