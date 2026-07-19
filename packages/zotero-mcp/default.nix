{ ... }: {
  den.aspects.packages.zotero-mcp = {
    overlays = _: {
      zotero-mcp = self: super: {
        zotero-mcp = super.callPackage ./package.nix { };
      };
    };
  };
}
