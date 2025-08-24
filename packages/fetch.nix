{ writeTextFile }:
writeTextFile {
  name = "fetch";
  executable = true;
  destination = "/bin/fetch";
  text = builtins.readFile ../fetch;
}
