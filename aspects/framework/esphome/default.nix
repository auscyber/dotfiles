# An `esphome` host class: den hosts that are microcontrollers.
#
# Same three tricks as ../mobile/: `class` is a free string, `instantiate` must
# be set (den's default throws on an unknown class), and `intoAttr` keeps nodes
# out of anything ../../tooling/ci-matrix.nix builds. Being a host puts a node
# in `hostsOf`, so the capability resolver can enumerate it.
#
# Node features are ordinary aspects writing `esphome` class content (see
# ../../esphome/); the base module tree every node gets is `esphome.modules`.
{
  lib,
  den,
  config,
  ...
}:
let
  top = config;
  inherit (lib) types mkOption;
in
{
  config.den.classes.esphome.description =
    "ESPHome node content: the component tree its firmware is compiled from.";

  options.esphome.modules = mkOption {
    type = types.attrsOf types.deferredModule;
    default = { };
    description = "The module tree every ESPHome node is evaluated with.";
  };

  options.flake.esphomeConfigurations = mkOption {
    default = { };
    type = types.lazyAttrsOf types.raw;
    description = "Devices declared with `class = \"esphome\"`; `.config.node.yaml` is the rendered tree.";
  };

  config.esphome.modules.base =
    { config, name, ... }:
    {
      options = {
        # Written by den's hostname battery and predicate aspects via `os`.
        networking.hostName = mkOption {
          type = types.str;
          default = name;
        };
        nixpkgs = mkOption {
          type = types.attrsOf types.raw;
          default = { };
        };

        node.name = mkOption {
          type = types.strMatching "[a-z0-9]([a-z0-9-]*[a-z0-9])?";
          default = config.networking.hostName;
          description = "`esphome.name`: the mDNS name and the YAML basename.";
        };

        node.friendlyName = mkOption {
          type = types.nullOr types.str;
          default = null;
        };

        node.platform = mkOption {
          type = types.enum [
            "esp32"
            "esp8266"
            "rp2040"
            "bk72xx"
            "rtl87xx"
            "ln882x"
            "nrf52"
            "host"
          ];
          description = "The platform block the board lives under. Features that need a platform pin it here.";
        };

        node.board = mkOption {
          type = types.str;
        };

        # Deep-merged across aspects. A YAML tag is a one-key attrset:
        # `{ "!secret" = "wifi_password"; }` renders `!secret wifi_password`.
        node.config = mkOption {
          type = types.attrsOf types.anything;
          default = { };
        };

        node.yaml = mkOption {
          type = types.raw;
          internal = true;
          readOnly = true;
        };
      };

      config.node.yaml = lib.recursiveUpdate config.node.config {
        esphome = {
          inherit (config.node) name;
        }
        // lib.optionalAttrs (config.node.friendlyName != null) {
          friendly_name = config.node.friendlyName;
        };
        ${config.node.platform}.board = config.node.board;
      };
    };

  config.flake.lib.esphomeNode =
    { name }:
    # den passes { modules; pkgs?; }; its `pkgs` is for a fictional system.
    { modules, ... }:
    let
      evaluated = lib.evalModules {
        specialArgs = { inherit name den; };
        modules = modules ++ lib.attrValues top.esphome.modules;
      };
    in
    {
      inherit (evaluated) config options extendModules;
      inherit name;
    };

  # `nix build .#esphome-<host>-yaml`, and `nix run .#esphome-<host>-check`
  # for an offline `esphome config` against placeholder secrets. The YAML is
  # symlinked into a writable dir because `!secret` resolves lexically beside it.
  config.perSystem =
    { pkgs, ... }:
    let
      fmt = pkgs.formats.yaml { tags = true; };
      perNode = lib.mapAttrsToList (
        host: node:
        let
          nodeName = node.config.node.name;
          yaml = fmt.generate "${nodeName}.yaml" node.config.node.yaml;
          check = pkgs.writeShellApplication {
            name = "esphome-${host}-check";
            runtimeInputs = [ pkgs.esphome ];
            text = ''
              d="$(mktemp -d)"
              trap 'rm -rf "$d"' EXIT
              ln -s ${yaml} "$d/${nodeName}.yaml"
              printf 'wifi_password: placeholder-not-the-real-passphrase\n' > "$d/secrets.yaml"
              cd "$d"
              ESPHOME_DATA_DIR="$d/.esphome" esphome config "${nodeName}.yaml"
            '';
          };
        in
        {
          packages."esphome-${host}-yaml" = yaml;
          packages."esphome-${host}-check" = check;
          apps."esphome-${host}-check" = {
            type = "app";
            program = lib.getExe check;
          };
        }
      ) top.flake.esphomeConfigurations;
    in
    {
      packages = lib.mergeAttrsList (map (n: n.packages) perNode);
      apps = lib.mergeAttrsList (map (n: n.apps) perNode);
    };
}
