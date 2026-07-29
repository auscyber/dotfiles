{ lib, ... }:
let
  inherit (lib)
    mkOptionType
    getExe
    getExe'
    isDerivation
    ;

  # Name for a derivation: meta.description → mainProgram → pname → name.
  drvName =
    drv:
    drv.meta.description or drv.meta.mainProgram or drv.pname or (builtins.parseDrvName drv.name).name
      or drv.name;

  # script = lib.getExe of the derivation.
  # Use getExe when meta.mainProgram is set; otherwise getExe' with the
  # derived name to avoid getExe's deprecation warning.
  drvScript =
    drv: if drv.meta.mainProgram or null != null then getExe drv else getExe' drv (drvName drv);

  # Build a { name, script; } record from a name + a derivation-or-string value.
  mkRecord =
    name: x:
    if isDerivation x then
      {
        inherit name;
        script = drvScript x;
      }
    else if builtins.isString x then
      {
        inherit name;
        script = x;
      }
    else
      throw "exeOrExes: values must be derivations or strings (got ${builtins.typeOf x})";

  # Flatten any allowed definition shape into a list of { name, script; }.
  # Shapes:
  #   - derivation            → name from meta.description/etc.
  #   - string                → name = script = the string
  #   - [ derivation|string ] → names derived per-element as above
  #   - attrset               → each attr NAME becomes `name`, value is drv|string
  toRecords =
    def:
    if isDerivation def then
      [ (mkRecord (drvName def) def) ]
    else if builtins.isString def then
      [
        {
          name = def;
          script = def;
        }
      ]
    else if builtins.isList def then
      builtins.map (
        el:
        if isDerivation el then
          mkRecord (drvName el) el
        else if builtins.isString el then
          {
            name = el;
            script = el;
          }
        else
          throw "exeOrExes: list elements must be derivations or strings"
      ) def
    else if builtins.isAttrs def then
      lib.mapAttrsToList (k: v: mkRecord k v) def
    else
      throw "exeOrExes: expected a derivation, a list, a string, or an attrset";

  hookType = mkOptionType {
    name = "exe-or-exes";
    description = "a derivation, a list of derivations/strings, a string, or an attrset of derivations/strings";
    descriptionClass = "noun";

    check =
      x:
      isDerivation x
      || builtins.isString x
      || (builtins.isList x && builtins.all (el: isDerivation el || builtins.isString el) x)
      || (
        builtins.isAttrs x
        && !isDerivation x
        && builtins.all (v: isDerivation v || builtins.isString v) (builtins.attrValues x)
      );

    merge = loc: defs: builtins.concatLists (builtins.map (d: toRecords d.value) defs);

    emptyValue = {
      value = [ ];
    };
  };
  makeHookText =
    hooks:
    lib.concatMapStringsSep "\n" (hook: ''
      echo "Running hook ${hook.name}..."
      echo "----------------------------------------"
      {
      ${hook.script}
      }
      echo "----------------------------------------"
    '') hooks;
in
{
  perSystem =
    {
      pkgs,
      config,
      ...
    }:
    {
      options.update-hooks = {
        preFlake = lib.mkOption {
          type = hookType;
          default = [ ];
        };
        flake = lib.mkOption {
          type = hookType;
          default = [ ];
        };
        postFlake = lib.mkOption {
          type = hookType;
          default = [ ];
        };
        finalPostParse = lib.mkOption {
          type = hookType;
          default = [ ];
        };
      };
      config = {
        apps.update = {
          type = "app";
          program = pkgs.writeShellScriptBin "update" ''
                                                                                                                                                                                                          #!${pkgs.runtimeShell}
                                                                                                                                                                                                          set -euo pipefail
                                                                                                                                                                                                          ${
                                                                                                                                                                                                            lib.optionalString
                                                                                                                                                                                                            (config.update-hooks.preFlake != [ ])
                                                                                                                                                                                                            ''
                                                                                                                                                                                                                echo "Running pre-flake hooks..."
                                                                                                                                                                                                              ${makeHookText config.update-hooks.preFlake}
                                                                                                                                                                                                            ''
                                                                                                                                                                                                          }
                                                                                                                                                                                                          echo "Updating flake inputs..."
                                                                                                                                                                                                          nix flake update
            ${lib.optionalString (config.update-hooks.postFlake != [ ]) ''
                echo "Running post-flake hooks..."
              ${makeHookText config.update-hooks.postFlake}
            ''}

            ${lib.optionalString (config.update-hooks.flake != [ ]) ''
                echo "Running interwined flake hooks..."
              ${makeHookText config.update-hooks.flake}
            ''}

            ${lib.optionalString (config.update-hooks.finalPostParse != [ ]) ''
                echo "Running final post-parse hooks..."
              ${makeHookText config.update-hooks.finalPostParse}
            ''}

          '';
        };
      };
    };
}
