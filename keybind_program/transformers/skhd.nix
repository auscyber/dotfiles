{config, lib, pkgs, isDarwin, ...}:
let 
  modifiers = import ./modifiers.nix { inherit isDarwin; };
  index = {
    "${modifiers.ctrl}" = "Ctrl";
    "${modifiers.alt}" = "Alt";
    "${modifiers.shift}" = "Shift";
    "${modifiers.super}" = "Super";
    "${modifiers.cmd}" = "Cmd";
    "${modifiers.defaultMod}" = if isDarwin then "Cmd" else "Super";
  };
  in
rec {
  name = "skhd";
  description = "A simple keybind manager for macOS, used to manage keybindings for various applications and window managers.";
  textFunction = keybinds:
    let
      skhdConfig =  ''
        # Keybinds for skhd
        ${lib.concatStringsSep "\n" (map (kb: "${kb.Key} ${if builtins.length kb.modifiers > 0 then "+ ${builtins.concatStringsSep " + " (builtins.map (m: "${index.${m}}") kb.modifiers)}" else ""} : ${kb.action}") keybinds)}
      '';
    in
    skhdConfig;
  function = keybinds:
    let
      skhdConfig = pkgs.writeText "skhd-config" textFunction keybinds;
    in
    pkgs.skhd.override {
      configFile = skhdConfig;
    };
}