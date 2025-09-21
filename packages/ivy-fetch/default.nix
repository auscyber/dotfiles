{ writeShellApplication, viu }:
writeShellApplication {
  name = "fetch";
  checkPhase = '''';
  bashOptions = [ ];
  runtimeInputs = [ viu ];
  text = builtins.readFile ./fetch;
}
