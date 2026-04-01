{ ... }:
{
  den.aspects.macmini = {
    darwin = { ... }: {
      imports = [ ../../systems/aarch64-darwin/macmini ];
    };
    # No dedicated home configuration for macmini; ivypierlot's base
    # homeManager aspect applies via den.aspects.ivypierlot.homeManager.
  };
}
