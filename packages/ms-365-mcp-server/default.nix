{ ... }: {
  den.aspects.packages.ms-365-mcp-server = {
    overlays = _: {
      ms-365-mcp-server = self: super: {
        ms-365-mcp-server = super.callPackage ./package.nix { };
      };
    };
  };
}
