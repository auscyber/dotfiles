{ ... }: {
  den.aspects.packages.todoist-mcp = {
    overlays = _: {
      todoist-mcp = self: super: {
        todoist-mcp = super.callPackage ./package.nix { };
      };
    };
  };
}
