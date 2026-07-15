{ ... }:
{
  den.aspects.packages.jj-mcp-server = {
    overlays = _: {
      jj-mcp-server = self: super: {
        jj-mcp-server = super.callPackage ./package.nix { };
      };
    };
  };
}
