{
  inputs,
}:

let
  lib = inputs.nixpkgs.lib;
dummyPubkey = "age1qyqszqgpqyqszqgpqyqszqgpqyqszqgpqyqszqgpqyqszqgpqyqs3290gq";
in

rec {

filterDummy = lib.filterAttrs (_: system:  system.config.age.rekey.hostPubkey != dummyPubkey || system.config.age.rekey.hostPubkey == null);
  overrideDerivation =
    drv: basePkgs: drv.override (builtins.intersectAttrs drv.override.__functionArgs basePkgs);
  displayLine =
    key: value: numIndents:
    let
      indent = builtins.concatStringsSep "" (builtins.genList (x: "│    ") numIndents);
    in
    if builtins.typeOf value == "set" then
      (
        if builtins.length (lib.attrsToList value) == 0 then
          null
        else
          "${indent}├── ${key}\n"
          + builtins.concatStringsSep "\n" (
            builtins.filter (x: x != null) (lib.mapAttrsToList (k: v: displayLine k v (numIndents + 1)) value)
          )
      )
    else if builtins.typeOf value == "list" then
      if builtins.length value == 0 then
        null
      else
        "${indent}├── ${key}"
        + builtins.concatStringsSep "\n" (
          builtins.filter (x: x != null) (builtins.map (v: displayLine "-" v (numIndents + 1)) value)
        )
    else if builtins.typeOf value == "null" then
      null
    else if builtins.typeOf value == "bool" then
      "${indent}├── ${key}  : ${if value then "enabled" else "disabled"}"
    else
      "${indent}├── ${key} : ${builtins.toString value}";

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
  toBase64 = text: let
    inherit (lib) sublist mod stringToCharacters concatMapStrings;
    inherit (lib.strings) charToInt;
    inherit (builtins) substring foldl' genList elemAt length concatStringsSep stringLength;
    lookup = stringToCharacters "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
    sliceN = size: list: n: sublist (n * size) size list;
    pows = [(64 * 64 * 64) (64 * 64) 64 1];
    intSextets = i: map (j: mod (i / j) 64) pows;
    compose = f: g: x: f (g x);
    intToChar = elemAt lookup;
    convertTripletInt = sliceInt: concatMapStrings intToChar (intSextets sliceInt);
    sliceToInt = foldl' (acc: val: acc * 256 + val) 0;
    convertTriplet = compose convertTripletInt sliceToInt;
    join = concatStringsSep "";
    convertLastSlice = slice: let
      len = length slice;
    in
      if len == 1
      then (substring 0 2 (convertTripletInt ((sliceToInt slice) * 256 * 256))) + "=="
      else if len == 2
      then (substring 0 3 (convertTripletInt ((sliceToInt slice) * 256))) + "="
      else "";
    len = stringLength text;
    nFullSlices = len / 3;
    bytes = map charToInt (stringToCharacters text);
    tripletAt = sliceN 3 bytes;
    head = genList (compose convertTriplet tripletAt) nFullSlices;
    tail = convertLastSlice (tripletAt nFullSlices);
  in
    join (head ++ [tail]);

}
