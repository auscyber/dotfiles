{
  pkgs ? import <nixpkgs> { },
}:
pkgs.polybar.override {
  githubSupport = true;
  pulseSupport = true;
}
