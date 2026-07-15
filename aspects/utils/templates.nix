{ lib, ... }:
{
  flake.templates =
    builtins.readDir ../../templates
    |> builtins.attrNames
    |> lib.flip lib.genAttrs' (name: {
      name = name;
      value = {
        path = ../../templates/${name};
      };
    });
}
