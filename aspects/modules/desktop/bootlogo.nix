{ den, ... }:
{
  den.aspects.bootlogo = {
    nixos = {
      boot.plymouth.enable = true;
    };
  };
}
