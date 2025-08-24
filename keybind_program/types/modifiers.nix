{ isDarwin}:
rec {
  ctrl = "ctrl";
  alt = "alt";
  shift = "shift";
  super = "super";
  cmd = "cmd";
  defaultMod = if isDarwin then cmd else super;
}