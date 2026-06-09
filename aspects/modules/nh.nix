{
  den,
  lib,
  inputs,
  ...
}:
{
  ff.nh.url = "github:auscyber/nh?ref=inputs/nh";
  den.default = {
    overlays = {
      nh = lib.optional (inputs ? nh) inputs.nh.overlays.default;

    };

  };
  perSystem = { pkgs, ... }: {
    packages = den.lib.nh.denPackages { fromFlake = true; } pkgs;
  };
}
