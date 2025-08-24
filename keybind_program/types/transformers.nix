{ config, lib, ... }:
with lib;
{
  options = {
    enable = mkOption {
      type = types.bool;
      default = false;
      description = "Enable the keybind control service.";
    };
    name = mkOption {
      type = types.str;
      default = "";
      description = "The name of the transformer.";
    };
    description = mkOption {
      type = types.str;
      default = "";
      description = "A description of the transformer for documentation purposes.";
    };
    textFunction = mkOption {
      type = types.functionTo types.any;
      default = "";
      description = "The text or configuration that the transformer will generate based on the keybinds.";
    };
    function = mkOption {
      type = types.functionTo types.deferredModule;
      default = "";
      description = "The function or command that the transformer will execute to convert keybinds.";
    };

  };
}