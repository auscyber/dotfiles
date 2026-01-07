{
  inputs,
}:

let
  lib = inputs.nixpkgs.lib;
in
rec {
  overrideDerivation =
    drv: basePkgs: drv.override (builtins.intersectAttrs drv.override.__functionArgs basePkgs);
  displayLine =
    key: value: numIndents:
    let
      indent = builtins.concatStringsSep "" (builtins.genList (x: "|    ") numIndents);
    in
    if builtins.typeOf value == "set" then
      (
        if builtins.length (lib.attrsToList value) == 0 then
          null
        else
          "${indent}| -- ${key}\n"
          + builtins.concatStringsSep "\n" (
            builtins.filter (x: x != null) (lib.mapAttrsToList (k: v: displayLine k v (numIndents + 1)) value)
          )
      )
    else if builtins.typeOf value == "list" then
      if builtins.length value == 0 then
        null
      else
        "${indent}| -- ${key}\n"
        + builtins.concatStringsSep "\n" (
          builtins.filter (x: x != null) (builtins.map (v: displayLine "-" v (numIndents + 1)) value)
        )
    else if builtins.typeOf value == "null" then
      null
    else if builtins.typeOf value == "bool" then
      "${indent}| -- ${key}  : ${if value then "enabled" else "disabled"}"
    else
      "${indent}| -- ${key} : ${builtins.toString value}";

  walkConfig =
    config:
    let

      cleanDrvs =
        attrs:
        if builtins.typeOf attrs == "set" then
          lib.filterAttrs (
            k: v:
            !lib.attrsets.isDerivation v
            && (if builtins.typeOf v == "set" then v ? enable && v.enable else true)
          ) attrs
        else
          attrs;
      # Helper function to display config lines with indentation like this:
      #  group
      #  | -- key1
      #  | -- key2
      #  |    | -- subkey1 : value

    in
    if builtins.typeOf config == "set" then
      (
        if config ? enable then
          (if config.enable then true else null)
        else
          lib.mapAttrs (k: v: walkConfig (cleanDrvs v)) config
      )
    else if builtins.typeOf config == "list" then
      lib.filter (v: !lib.attrsets.isDerivation v) config
    else
      config;

}
