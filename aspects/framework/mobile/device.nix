{ lib, ... }:
let
  inherit (lib) types mkOption;
in
{
  config.mobile.modules.device =
    { config, name, ... }:
    {
      options = {
        # den's hostname battery writes this through `os` -> `mobile`.
        networking.hostName = mkOption {
          type = types.str;
          default = name;
          description = "The device's hostname, as seen on the tailnet.";
        };

        # den's predicate aspects write here through `os`. Declared, not
        # freeform: a freeform `config` made readers self-referential.
        nixpkgs = mkOption {
          type = types.attrsOf types.raw;
          default = { };
        };

        assertions = mkOption {
          type = types.listOf (
            types.submodule {
              options.assertion = mkOption { type = types.bool; };
              options.message = mkOption { type = types.str; };
            }
          );
          default = [ ];
        };

        device = {
          description = mkOption {
            type = types.str;
            default = name;
          };

          # What devicectl and Finder match on, e.g. "Ivy’s iPhone".
          deviceName = mkOption {
            type = types.str;
            default = name;
          };

          kind = mkOption {
            type = types.enum [
              "iphone"
              "ipad"
            ];
            default = "iphone";
          };

          # What Shortcuts' "Get Device Details -> Device Model" reports.
          model = mkOption {
            type = types.str;
            readOnly = true;
            default =
              {
                iphone = "iPhone";
                ipad = "iPad";
              }
              .${config.device.kind};
          };

          tailscale.enable = mkOption {
            type = types.bool;
            default = false;
            description = "Joins the tailnet by a key minted with `.#tailscale-client-key`.";
          };

          notes = mkOption {
            type = types.listOf types.str;
            default = [ ];
            description = "What a human has to do by hand, shown by `.bundle`.";
          };
        };

        bundle = mkOption {
          type = types.str;
          readOnly = true;
        };
      };

      config.bundle = ''
        ${config.device.description} (${name})

        ${lib.concatMapStringsSep "\n" (n: "  - ${n}") config.device.notes}
      '';
    };
}
