{ ... }:
{
  den.aspects."Ivys-MacBook-Pro" = {
    darwin = { ... }: {
      imports = [ ../../systems/aarch64-darwin/Ivys-MacBook-Pro ];
    };

    provides.ivypierlot = { ... }: {
      homeManager.imports = [ (../../homes/aarch64-darwin + "/ivypierlot@Ivys-MacBook-Pro") ];
    };
  };
}
