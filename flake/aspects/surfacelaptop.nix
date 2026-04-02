{ ... }:
{
  den.aspects.surfacelaptop = {
    nixos = { ... }: {
      imports = [ ../../systems/x86_64-linux/surfacelaptop ];
    };

    provides.auscyber = { ... }: {
      homeManager.imports = [ (../../homes/x86_64-linux + "/auscyber@surfacelaptop") ];
    };
  };
}
