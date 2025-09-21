{ stdenv }:
rec {
  modifiers = import ./modifiers.nix { inherit (stdenv) isDarwin; };
  skhd = rec {
    inherit modifiers;
    runSkhdCommand =
      {
        key,
        modifiers ? [ ],
        command ? "",
        description ? "",
        group ? null,
      }:
      {
        inherit
          key
          modifiers
          command
          description
          group
          ;
        action = command;
      };

  };

}
