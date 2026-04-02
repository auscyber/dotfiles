{ ... }:
{
  den.aspects.macmini = {
    darwin = { ... }: {
      imports = [ ../../systems/aarch64-darwin/macmini ];
    };

    # ivypierlot on macmini: the shared feature aspects arrive via
    # den.aspects.ivypierlot.includes; only macmini-specific home-manager
    # additions are placed here.
    provides.ivypierlot = { ... }: {
      homeManager = { ... }: { };
    };
  };
}
