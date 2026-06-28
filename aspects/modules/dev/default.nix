{ den, ... }:
{
  den.aspects.dev = {
    includes = [
      den.aspects.dev-nix
      den.aspects.dev-cli
    ];
  };
}
