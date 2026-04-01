{ ... }:
{
  den.aspects.secondpc = {
    nixos = { ... }: {
      imports = [ ../../systems/x86_64-linux/secondpc ];
    };

    provides.auscyber = { ... }: {
      homeManager.imports = [ (../../homes/x86_64-linux + "/auscyber@secondpc") ];
    };
  };
}
