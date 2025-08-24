{ config, lib, pkgs, ... }:
with lib;

{
  options = {
    Key = mkOption {
      type = types.str;
      default = "";
      description = "The key or key combination to bind.";
    };
    action = mkOption {
      type = types.any;
      default = "";
      description = "The action to perform when the key is pressed.";
    };
    modifiers = mkOption {
      type = types.listOf (types.enum (builtins.attrNames (import ./modifiers.nix { inherit (pkgs.stdenv) isDarwin; })));
      default = [ ];
      description = "A list of modifiers (like Ctrl, Alt, Shift) that must be pressed along with the key.";
    };
    description = mkOption {
      type = types.str;
      default = "";
      description = "A description of the keybind for documentation purposes.";
    };
    group = mkOption {
      type = types.nullOr (types.str);
      default = null;
      description = "A group name for organizing keybinds. If null, the keybind is not grouped.";
    };

    application = mkOption {
      type = types.nullOr (types.str);
      default = null;
      description = "The application or context in which this keybind is applicable. If null, the keybind is global.";
    };

  };

}
